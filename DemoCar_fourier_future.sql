/* Step 1. Set up Schema*/
USE DemoSales;

DROP TABLE IF EXISTS prediction_data, error, forecast_data;
CREATE TABLE prediction_data(
				"Account" NVARCHAR(50) NOT NULL
				,"BusinessProcess" NVARCHAR(50) NOT NULL
				,"Entity" NVARCHAR(50) NOT NULL
				,"LineItemDetail" NVARCHAR(50) NOT NULL
				,"Product" NVARCHAR(50) NOT NULL
				,"Scenario" NVARCHAR(50) NOT NULL
				,"Time" Date NOT NULL
				,"TimeDataView" NVARCHAR(50) NOT NULL
				,"Finance_Value" FLOAT NOT NULL
				,"LinearRegression" FLOAT NOT NULL
				,"DecisionTree" FLOAT NOT NULL
				,"ConditionalTree" FLOAT NOT NULL
				,"RandomForest" FLOAT NOT NULL
				);

CREATE TABLE error (
				"Model Name" VARCHAR(100) NOT NULL,
				"MAPE" FLOAT NOT NULL);


CREATE TABLE forecast_data(
				"Account" NVARCHAR(50) NOT NULL
				,"BusinessProcess" NVARCHAR(50) NOT NULL
				,"Entity" NVARCHAR(50) NOT NULL
				,"LineItemDetail" NVARCHAR(50) NOT NULL
				,"Product" NVARCHAR(50) NOT NULL
				,"Scenario" NVARCHAR(50) NOT NULL
				,"Time" Date NOT NULL
				,"TimeDataView" NVARCHAR(50) NOT NULL
				,"Forecast" FLOAT NOT NULL)

GO

TRUNCATE TABLE prediction_data;
INSERT INTO prediction_data("Account", "BusinessProcess", "Entity", "LineItemDetail", "Product", "Scenario", "Time", "TimeDataView", "Finance_Value", "LinearRegression","DecisionTree", "ConditionalTree", "RandomForest")
EXEC sp_execute_external_script
					@language = N'R'
					,@script = N'
library(DataCombine)
library(randomForest)
#library(ridge)
library(glmnet)
library(gbm)
library(rpart)
library(party)
library(caret)
library(xgboost)
library(forecast)
library(tseries)
data <- data.frame(data)

######Function to determine Auto-correlated factor#############
ssacf<- function(x) sum(acf(x, na.action = na.omit, plot = FALSE)$acf^2)

######Function to compare additive and multiplicative factors##########
compare_ssacf<-function(add,mult) ifelse(ssacf(add)< ssacf(mult), 
                                         "Additive", "Multiplicative") 

######### Function to determine if Additive or multiplicative###########
additive_or_multiplicative <- function(dt,Target_col){
  dt$trend <- rollmean(Target_col, 3, fill="extend", align = "right")
  dt$additive_detrended <- Target_col - dt$trend
  dt$muliplicative_detrended <- Target_col / dt$trend
  dt$additive_seasonal <- mean(dt$additive_detrended, na.rm = TRUE)
  dt$multiplicative_seasonal <- mean(dt$muliplicative_detrended, na.rm = TRUE)
  dt$additive_residual <- dt$additive_detrended - dt$additive_seasonal
  dt$multiplicative_residual <- dt$muliplicative_detrended / dt$multiplicative_seasonal
  TS_type <- compare_ssacf(dt$additive_residual,dt$multiplicative_residual)
  return(TS_type)
}

########Function to add extra features############
train_test_forecast<-function(data){
  data$year<-as.numeric(format(as.Date(data$Time), "%Y"))
  data$week_day<-as.numeric(format(as.Date(data$Time), "%w"))
  data$month<-as.numeric(format(as.Date(data$Time), "%m"))
  data$week<-as.numeric(format(as.Date(data$Time), "%W"))
  data$day_of_month <- as.numeric(format(as.Date(data$Time), "%d"))
  data_msts <- msts(data$Finance_Value, seasonal.periods = c(12,12*2))
  data_seas<-t(matrix(data=data$Finance_Value))
  data$seas<-colMeans(data_seas,na.rm=T)
  K <- 2
  data$fourier <- fourier(data_msts, K = c(K, K))
  
  
  #index <- 1:(floor(nrow(data)*0.8))
  index <- 1:(nrow(data) - 6)
  testset<-data[-index,]
  #new_testset<-subset(testset,select=-c(Finance_Value))
  trainset<-data[index,]
  
  data_trend <- ma(trainset$Finance_Value,order=3)
  #data_trend2 <- rollmean(trainset$Finance_Value, 3, fill="extend", align = "right")
  #decomp2 <- stl(ts(trainset[,"Finance_Value"], frequency=1),s.window = "periodic", robust = TRUE)$time.series

  TS_type <- additive_or_multiplicative(trainset,trainset$Finance_Value)
  
  if(TS_type=="Multiplicative") trainset$detrended <- trainset$Finance_Value/data_trend
  if(TS_type=="Additive") trainset$detrended <- trainset$Finance_Value-data_trend
  
  
  trainset<-trainset[complete.cases(trainset),]

  trend_fit<-auto.arima(data_trend)
  trend_for<-as.matrix(forecast(trend_fit,floor(nrow(data)*0.2))$mean)
  regress_form <- detrended~year+week_day+month+week+day_of_month+seas
  
  x_train <- as.matrix(subset(trainset,select = -c(Account, BusinessProcess, Entity, LineItemDetail, Product, Scenario, Time, TimeDataView, Finance_Value, detrended))) #Removes Target
  y_train <- as.matrix(subset(trainset,select = c(detrended))) #Only Target
  x_test <- as.matrix(subset(testset, select = -c(Account, BusinessProcess, Entity, LineItemDetail, Product, Scenario, Time, TimeDataView, Finance_Value)))

  #Linear Regression
  lr_model <- rxLinMod(regress_form, data = trainset)
  testset$LinearRegression <- as.matrix(rxPredict(lr_model, testset))

  #Regular Decision Tree - Working
  dTree <- rpart(regress_form, method="anova",data= trainset)
  testset$DecisionTree <- as.matrix(predict(dTree,testset))
  
  #Conditional Inference Tree
  cTree <- ctree(as.formula(regress_form), data=trainset)
  testset$ConditionalTree <- as.matrix(predict(cTree,testset))
  
  #Regular Random Forest - Working
  rf <- randomForest(as.formula(regress_form), data = data.frame(trainset), ntree = 50, nodesize = 3, importance = TRUE)
  testset$RandomForest <- as.matrix(predict(rf,testset))

  if(TS_type=="Multiplicative"){
	testset$LinearRegression <- testset$LinearRegression*as.vector(trend_for)
	testset$DecisionTree <- testset$DecisionTree*as.vector(trend_for)
	testset$ConditionalTree <- testset$ConditionalTree*as.vector(trend_for)
	testset$RandomForest <- testset$RandomForest*as.vector(trend_for)
  }
  if(TS_type=="Additive"){
	testset$LinearRegression <- testset$LinearRegression+as.vector(trend_for)
	testset$DecisionTree <- testset$DecisionTree+as.vector(trend_for)
	testset$ConditionalTree <- testset$ConditionalTree+as.vector(trend_for)
	testset$RandomForest <- testset$RandomForest+as.vector(trend_for)
  }

  testset <- subset(testset, select = c(Account, BusinessProcess, Entity, LineItemDetail, Product, Scenario, Time, TimeDataView, Finance_Value,LinearRegression,DecisionTree,ConditionalTree,RandomForest))
  rownames(testset) <- c()
  return(testset)

}

predictions <- data.frame(matrix(ncol = 16, nrow = 0))
#combo <- unique(data[,c("Entity","Product")])
#for(i in seq(1,nrow(combo))){
  #temp <- data[data$Entity == combo[i,1] & data$Product == combo[i,2],]
  temp <- data[data$Entity == "LE6" & data$Product == "A100",]
  temp <- temp[order(temp$Time),]
  pred <- as.matrix(train_test_forecast(temp))
  predictions <<- rbind(predictions,pred)
    
#}
'
					,@input_data_1 = N'SELECT * FROM Data;'
					,@input_data_1_name = N'data'
					,@output_data_1_name = N'predictions'
--WITH result SETS (("Time" Date, "Entity" NVARCHAR(100), "Product" NVARCHAR(100), "Quantity" FLOAT, "Prediction" FLOAT ));
SELECT * FROM prediction_data;
GO

CREATE OR ALTER PROCEDURE calc_error
AS
BEGIN
TRUNCATE TABLE error;			
INSERT INTO error("Model Name", "MAPE")
			EXEC sp_execute_external_script
								@language = N'R'
								,@script = N'
error <- function(actual,pred){
	return(mean(rowMeans(abs((actual-pred)/actual) * 100)))}
modelname <- c("Linear Regression", "Decision Tree", "Conditional Tree", "Random Forest")
actual <- data.frame(avp$Finance_Value)
errors <- c(error(actual,avp$LinearRegression), error(actual,avp$DecisionTree),error(actual,avp$ConditionalTree), error(actual,avp$RandomForest))
OutputDataSet <- data.frame(modelname, errors)'
			,@input_data_1 = N'Select * FROM prediction_data'
			,@input_data_1_name = N'avp'
--WITH result SETS (("MAPE" FLOAT));
SELECT * FROM error ORDER BY MAPE;
END;
GO

calc_error

CREATE OR ALTER PROCEDURE forecast @modelname NVARCHAR(50), @forecast_period INT
AS
BEGIN
TRUNCATE TABLE forecast_data;
INSERT INTO forecast_data("Account","BusinessProcess","Entity", "LineItemDetail","Product", "Scenario","Time", "TimeDataView", "Forecast")
			EXEC sp_execute_external_script
								@language = N'R'
								,@script = N'
library(DataCombine)
library(randomForest)
#library(ridge)
library(glmnet)
library(gbm)
library(rpart)
library(party)
library(caret)
library(xgboost)
library(forecast)
library(tseries)
data <- data.frame(data)
#Function to determine Auto-correlated factor
ssacf<- function(x) sum(acf(x, na.action = na.omit, plot = FALSE)$acf^2)
#Function to compare additive and multiplicative factors
compare_ssacf<-function(add,mult) ifelse(ssacf(add)< ssacf(mult), 
                                         "Additive", "Multiplicative") 
# Function to determine if Additive or multiplicative
additive_or_multiplicative <- function(dt,Target_col){
  dt$trend <- rollmean(Target_col, 3, fill="extend", align = "right")
  dt$additive_detrended <- Target_col - dt$trend
  dt$muliplicative_detrended <- Target_col / dt$trend
  dt$additive_seasonal <- mean(dt$additive_detrended, na.rm = TRUE)
  dt$multiplicative_seasonal <- mean(dt$muliplicative_detrended, na.rm = TRUE)
  dt$additive_residual <- dt$additive_detrended - dt$additive_seasonal
  dt$multiplicative_residual <- dt$muliplicative_detrended / dt$multiplicative_seasonal
  TS_type <- compare_ssacf(dt$additive_residual,dt$multiplicative_residual)
  return(TS_type)
}

#Function to add extra features
train_test_forecast<-function(data){
  data$year<-as.numeric(format(as.Date(data$Time), "%Y"))
  data$week_day<-as.numeric(format(as.Date(data$Time), "%w"))
  data$month<-as.numeric(format(as.Date(data$Time), "%m"))
  data$week<-as.numeric(format(as.Date(data$Time), "%W"))
  data$day_of_month <- as.numeric(format(as.Date(data$Time), "%d"))
  
  #forecast fourier series
  data_msts <- msts(data$Finance_Value, seasonal.periods = c(12,12*5))
  #K <- nrow(data)/forecast_period #2 - K must be not be greater than period/2
  K = c(4,8)
  data$fourier <- fourier(data_msts, K = K)
  for_fourier <- fourier(data_msts, K = K, h=forecast_period)
  data <- data.frame(data,data$fourier)
  #print(data)

  #forecast seasonality lag
  #data_seas<-t(matrix(data=data$Finance_Value))
  #data$seas<-colMeans(data_seas,na.rm=T)
  #print(str(data$seas)
  #seas_fit <- auto.arima(data$seas)
  #for_seas <- forecast(seas_fit,12)$mean

  
  data_trend <- ma(data$Finance_Value,order=3)
  #data_trend2 <- rollmean(data$Finance_Value, 3, fill="extend", align = "right")
  #decomp2 <- stl(ts(data[,"Finance_Value"], frequency=1),s.window = "periodic", robust = TRUE)$time.series

  TS_type <- additive_or_multiplicative(data,data$Finance_Value)
  
  if(TS_type=="Multiplicative") data$detrended <- data$Finance_Value/data_trend
  if(TS_type=="Additive") data$detrended <- data$Finance_Value-data_trend
  #print(data)
  data1<-data
  #print("Here is data1:*************##########$$$$$$$")
  #print(data1)
  last_date <- as.Date(data[nrow(data),"Time"])
  data<-data[complete.cases(data),]

  trend_fit<-auto.arima(data_trend)
  trend_for<-as.matrix(forecast(trend_fit,60)$mean)
  regress_form <- detrended~year+week_day+month+week+day_of_month
  
    
  #Create data for forecast
  print("Number of rowS : *******")
  print(data$Time)
  print(nrow(data1))
  #last_date <- as.Date(data[nrow(data1),"Time"])
  print(last_date)
  forecast_start_date <- seq(last_date, by="month", length.out = forecast_period)[2]

  print("Here it is ##########3")
  print(forecast_start_date)
  forecasttest <- data.frame(Time = seq(forecast_start_date, by = "month", length.out = 60))
  forecasttest$Account <- data[1,"Account"]
  forecasttest$BusinessProcess <- data[1,"BusinessProcess"]
  forecasttest$Entity <- data[1,"Entity"]
  forecasttest$LineItemDetail <- data[1,"LineItemDetail"]
  forecasttest$Product <- data[1,"Product"]
  forecasttest$Scenario <- data[1,"Scenario"]
  forecasttest$TimeDataView <- data[1,"TimeDataView"]
  forecasttest$year <- as.numeric(format(as.Date(forecasttest$Time), "%Y"))
  forecasttest$week_day <- as.numeric(format(as.Date(forecasttest$Time), "%w"))
  forecasttest$month <- as.numeric(format(as.Date(forecasttest$Time), "%m"))
  forecasttest$week <- as.numeric(format(as.Date(forecasttest$Time), "%W"))
  forecasttest$day_of_month <- as.numeric(format(as.Date(forecasttest$Time), "%d"))
  #print(forecasttest$Time)
  x_train <- as.matrix(subset(data,select = -c(Account, BusinessProcess, Entity, LineItemDetail, Product, Scenario, Time, TimeDataView, Finance_Value, detrended))) #Removes Target
  y_train <- as.matrix(subset(data,select = c(detrended))) #Only Target
  x_test <- as.matrix(subset(forecasttest, select = -c(Account, BusinessProcess, Entity, LineItemDetail, Product, Scenario, Time, TimeDataView)))

  model_list <- list("Linear Regression" = lm(regress_form, data = data),
				"Decision Tree" = rpart(regress_form, method="anova",data= data), 
				#"Conditional Tree" = ctree(as.formula(regress_form), data=data), 
				"Random Forest" = randomForest(as.formula(regress_form), data = data.frame(data), ntree = 50, nodesize = 3, importance = TRUE))
  predict_list <- list("Linear Regression" = as.matrix(predict(model_list[["Linear Regression"]], forecasttest)),
					   "Decision Tree" = as.matrix(predict(model_list[["Decision Tree"]],forecasttest)),
					#   "Conditional Tree" = as.matrix(predict(model_list[["Conditional Tree"]],forecasttest)),
					   "Random Forest" = as.matrix(predict(model_list[["Random Forest"]],forecasttest)))
  
  
  forecasttest$Forecast <- predict_list[[modelname]]

  if(TS_type=="Multiplicative") forecasttest$Forecast <- forecasttest$Forecast*trend_for
  if(TS_type=="Additive") forecasttest$Forecast <- forecasttest$Forecast+trend_for

  forecasttest <- subset(forecasttest, select = c(Account, BusinessProcess,Entity, LineItemDetail, Product, Scenario, Time, TimeDataView,Forecast))
  rownames(forecasttest) <- c()
  return(forecasttest)
  
} 

predictions <- data.frame(matrix(ncol = 8, nrow = 0))
#combo <- unique(train[,c("Entity", "Product")])
#for(i in seq(1,nrow(combo))){
  #temp <- train[train$Entity == combo[i,1] & train$Product == combo[i,2],]
  temp <- train[train$Entity == "LE6" & train$Product == "A100",]
  temp <- temp[order(temp$Time),]
  pred <- as.matrix(train_test_forecast(temp))
  predictions <<- rbind(predictions,pred)
    
#}


'
			,@input_data_1 = N'SELECT * FROM Data'
			,@input_data_1_name = N'train'
			,@output_data_1_name = N'predictions'
			,@params = N'@modelname NVARCHAR(50), @forecast_period INT'
			,@modelname = @modelname
			,@forecast_period = @forecast_period
			
--WITH result SETS (("MAPE" FLOAT));
END;
GO

EXEC forecast @modelname = 'Random Forest', @forecast_period = 60;
Select * from forecast_data
   

