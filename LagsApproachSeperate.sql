/* Step 1. Set up Schema*/
USE master;

DROP TABLE IF EXISTS prediction_data, prediction_data_ts, error, error_ts, forecast_data;
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
				,"XGBoost" FLOAT NOT NULL
				);

CREATE TABLE prediction_data_ts(
				"Account" NVARCHAR(50) NOT NULL
				,"BusinessProcess" NVARCHAR(50) NOT NULL
				,"Entity" NVARCHAR(50) NOT NULL
				,"LineItemDetail" NVARCHAR(50) NOT NULL
				,"Product" NVARCHAR(50) NOT NULL
				,"Scenario" NVARCHAR(50) NOT NULL
				,"Time" Date NOT NULL
				,"TimeDataView" NVARCHAR(50) NOT NULL
				,"Finance_Value" FLOAT NOT NULL
				,"SimpleMovingAverage" NVARCHAR(50) NOT NULL
				,"ExponentialSmoothing" NVARCHAR(50) NOT NULL
				,"DoubleExponentialSmoothing" NVARCHAR(50) NOT NULL
				,"ETS" NVARCHAR(50) NOT NULL
				);

CREATE TABLE error (
				"Model Name" VARCHAR(100) NOT NULL
				,"MAPE" FLOAT NOT NULL
				,"MAE" FLOAT NOT NULL
				,"RMSE" FLOAT NOT NULL);

CREATE TABLE error_ts (
				"Model Name" VARCHAR(100) NOT NULL
				,"MAPE" FLOAT NOT NULL
				,"MAE" FLOAT NOT NULL
				,"RMSE" FLOAT NOT NULL);

CREATE TABLE forecast_data(
				"Account" NVARCHAR(50) NOT NULL
				,"BusinessProcess" NVARCHAR(50) NOT NULL
				,"Entity" NVARCHAR(50) NOT NULL
				,"LineItemDetail" NVARCHAR(50) NOT NULL
				,"Product" NVARCHAR(50) NOT NULL
				,"Scenario" NVARCHAR(50) NOT NULL
				,"Time" Date NOT NULL
				,"TimeDataView" NVARCHAR(50) NOT NULL
				,"LinearRegression" FLOAT NOT NULL
				,"RandomForest" FLOAT NOT NULL
				,"XGBoost" FLOAT NOT NULL
				)


GO

/* Process input dataset before splitting*/
CREATE OR ALTER PROCEDURE Predict_n_Evaluate
AS
BEGIN
TRUNCATE TABLE prediction_data;
INSERT INTO prediction_data("Account", "BusinessProcess", "Entity", "LineItemDetail", "Product", "Scenario", "Time", "TimeDataView", "Finance_Value", "LinearRegression", "DecisionTree", "ConditionalTree", "RandomForest", "XGBoost")
EXEC sp_execute_external_script
					@language = N'R'
					,@script = N'
library(DataCombine)
library(randomForest)
library(glmnet)
library(gbm)
library(rpart)
library(party)
library(caret)
library(xgboost)
library(forecast)
library(tseries)
data <- data.frame(data)

train_test <- function(data){

  data$year<-as.numeric(format(as.Date(data$Time), "%Y"))
  data$week_day<-as.numeric(format(as.Date(data$Time), "%w"))
  data$month<-as.numeric(format(as.Date(data$Time), "%m"))
  data$week<-as.numeric(format(as.Date(data$Time), "%W"))
  data$day_of_month <- as.numeric(format(as.Date(data$Time), "%d"))
  
  
  data_seas<-t(matrix(data=data$Finance_Value))
  data$seas<-colMeans(data_seas,na.rm=T)

  #forecast fourier series
  data_msts <- msts(data$Finance_Value, seasonal.periods = c(12))
  K <- 2 #2 - K must be not be greater than period/2
  Fourier <- data.frame(fourier(data_msts, K = K))
  colnames(Fourier) = c("FourierS1", "FourierC1", "FourierS2", "FourierC2")
  data <- data.frame(data,Fourier)


  regress_form <- "Finance_Value~"
  for(i in seq(1:6)){
    data <- slide(data, "Finance_Value" , NewVar = paste0("Lag_",i), slideBy = -i)  # create lag variables
	regress_form <- ifelse(i==1,paste(regress_form,"Lag_",i,sep=""),paste(regress_form,"+","Lag_",i,sep=""))
    # data <- slide(data, "Beer", NewVar = ld, slideBy = i)  # create lead1 variable
  }

  regress_form <- paste(regress_form,"+seas+FourierS1+FourierC1+FourierS2+FourierC2+year+week_day+month+week+day_of_month",sep="")

  #Remove NAs from the dataset
  clean_data <- data[complete.cases(data), ]
  #split
  index <- 1:(floor(nrow(clean_data)*0.2))
  testset <- clean_data[-index,]
  new_testset <-subset(testset, select = -c(Finance_Value))
  trainset <- clean_data[index,]
  #x_train <- as.matrix(subset(trainset,select = -c(Account, BusinessProcess, Entity, LineItemDetail, Scenario, Time, TimeDataView, Finance_Value, ChangeDatetime, Userid))) #Removes Target
  #y_train <- as.matrix(subset(trainset,select = c(Finance_Value))) #Only Target
  #x_test <- as.matrix(subset(new_testset, select = -c(Account, BusinessProcess, Entity, LineItemDetail, Product, Scenario, Time, TimeDataView, ChangeDatetime, Userid)))
  
  x_train <- model.matrix(~Lag_1+Lag_2+Lag_3+Lag_4+Lag_5+Lag_6+seas+FourierS1+FourierC1+FourierS2+FourierC2+year+week_day+month+week+day_of_month,trainset) #Removes Target
  y_train <- as.matrix(subset(trainset,select = c(Finance_Value))) #Only Target
  x_test <- model.matrix(~Lag_1+Lag_2+Lag_3+Lag_4+Lag_5+Lag_6+seas+FourierS1+FourierC1+FourierS2+FourierC2+year+week_day+month+week+day_of_month,new_testset)
  
  
  ####################Regression Models#####################################
  

  #Rx Linear Regresssion - Working
  lr_model <- rxLinMod(regress_form, data = trainset);
  testset$LinearRegression <- as.matrix(rxPredict(lr_model, new_testset))
  
  #Ridge Regression
  #ridgeReg <- cv.glmnet(data.matrix(x_train),y_train,alpha=0)
  #testset$RidgeRegression <- as.matrix(predict(ridgeReg, newx=data.matrix(x_test), s = "lambda.min"))

  #Lasso Regression
  #lassoReg <- cv.glmnet(data.matrix(x_train),y_train,alpha=1)
  #testset$LassoRegression <- as.matrix(predict(lassoReg, newx=data.matrix(x_test), s = "lambda.min"))
  
  #Regular Decision Tree - Working
  dTree <- rpart(regress_form, control = rpart.control(cp = 0.01, minsplit = 5, maxdepth = 4), data= trainset)
  testset$DecisionTree <- as.matrix(predict(dTree,new_testset))

  #Conditional Inference Tree
  cTree <- ctree(as.formula(regress_form), data=trainset)
  testset$ConditionalTree <- as.matrix(predict(cTree,new_testset))

  #Regular Random Forest - Working
  rf <- randomForest(as.formula(regress_form), data = data.frame(trainset), ntree = 100, nodesize = 3, importance = TRUE)
  testset$RandomForest <- as.matrix(predict(rf,new_testset))

  #Regular XGBoost
  xgb <- xgboost(data= xgb.DMatrix(data = data.matrix(trainset), label = trainset$Finance_Value), nfold = 5,nrounds = 5, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", max_depth = 3, nthread = 4)
  testset$XGBoost <- predict(xgb,data.matrix(new_testset))

  testset <- subset(testset, select = c(Account, BusinessProcess, Entity, LineItemDetail, Product, Scenario, Time, TimeDataView, Finance_Value,LinearRegression, DecisionTree,ConditionalTree,RandomForest,XGBoost))
  rownames(testset) <- c()
  return(testset)
}
  
predictions <- data.frame(matrix(ncol = 14, nrow = 0))
combo <- unique(data[,c("Entity","Product")])
for(i in seq(1,nrow(combo))){
  temp <- data[data$Entity == combo[i,1] & data$Product == combo[i,2],]
  temp <- temp[order(temp$Time),]
  pred <- as.matrix(train_test(temp))
  predictions <<- rbind(predictions,pred)
  }
'
					,@input_data_1 = N'SELECT * FROM DemoCarSalesData;'
					,@input_data_1_name = N'data'
					,@output_data_1_name = N'predictions'
--WITH result SETS (("Time" Date, "Entity" NVARCHAR(100), "Product" NVARCHAR(100), "Finance_Value" FLOAT, "Prediction" FLOAT ));
SELECT * FROM prediction_data;
END;
GO
EXEC Predict_n_Evaluate
CREATE OR ALTER PROCEDURE Predict_n_EvaluateTS
AS
BEGIN
TRUNCATE TABLE prediction_data_ts;
INSERT INTO prediction_data_ts("Account", "BusinessProcess", "Entity", "LineItemDetail", "Product", "Scenario", "Time", "TimeDataView", "Finance_Value", "SimpleMovingAverage","ExponentialSmoothing","DoubleExponentialSmoothing","ETS")
EXEC sp_execute_external_script
					@language = N'R'
					,@script = N'
library(DataCombine)
library(randomForest)
library(glmnet)
library(gbm)
library(rpart)
library(party)
library(caret)
library(xgboost)
library(forecast)
library(tseries)
data <- data.frame(data)

train_test <- function(data){

  data$year<-as.numeric(format(as.Date(data$Time), "%Y"))
  data$week_day<-as.numeric(format(as.Date(data$Time), "%w"))
  data$month<-as.numeric(format(as.Date(data$Time), "%m"))
  data$week<-as.numeric(format(as.Date(data$Time), "%W"))
  data$day_of_month <- as.numeric(format(as.Date(data$Time), "%d"))
  
  
  data_seas<-t(matrix(data=data$Finance_Value))
  data$seas<-colMeans(data_seas,na.rm=T)

  #forecast fourier series
  data_msts <- msts(data$Finance_Value, seasonal.periods = c(12))
  K <- 2 #2 - K must be not be greater than period/2
  Fourier <- data.frame(fourier(data_msts, K = K))
  colnames(Fourier) = c("FourierS1", "FourierC1", "FourierS2", "FourierC2")
  data <- data.frame(data,Fourier)


  regress_form <- "Finance_Value~"
  for(i in seq(1:6)){
    data <- slide(data, "Finance_Value" , NewVar = paste0("Lag_",i), slideBy = -i)  # create lag variables
	regress_form <- ifelse(i==1,paste(regress_form,"Lag_",i,sep=""),paste(regress_form,"+","Lag_",i,sep=""))
    # data <- slide(data, "Beer", NewVar = ld, slideBy = i)  # create lead1 variable
  }

  regress_form <- paste(regress_form,"+seas+FourierS1+FourierC1+FourierS2+FourierC2+year+week_day+month+week+day_of_month",sep="")

  #Remove NAs from the dataset
  clean_data <- data[complete.cases(data), ]
  #split
  index <- 1:(floor(nrow(clean_data)*0.2))
  testset <- clean_data[-index,]
  new_testset <-subset(testset, select = -c(Finance_Value))
  trainset <- clean_data[index,]
  #x_train <- as.matrix(subset(trainset,select = -c(Account, BusinessProcess, Entity, LineItemDetail, Scenario, Time, TimeDataView, Finance_Value, ChangeDatetime, Userid))) #Removes Target
  #y_train <- as.matrix(subset(trainset,select = c(Finance_Value))) #Only Target
  #x_test <- as.matrix(subset(new_testset, select = -c(Account, BusinessProcess, Entity, LineItemDetail, Product, Scenario, Time, TimeDataView, ChangeDatetime, Userid)))
  
  x_train <- model.matrix(~Lag_1+Lag_2+Lag_3+Lag_4+Lag_5+Lag_6+seas+FourierS1+FourierC1+FourierS2+FourierC2+year+week_day+month+week+day_of_month,trainset) #Removes Target
  y_train <- as.matrix(subset(trainset,select = c(Finance_Value))) #Only Target
  x_test <- model.matrix(~Lag_1+Lag_2+Lag_3+Lag_4+Lag_5+Lag_6+seas+FourierS1+FourierC1+FourierS2+FourierC2+year+week_day+month+week+day_of_month,new_testset)
  
  
  ####################Regression Models#####################################
  

  #create time series
  TS <- ts(data$Finance_Value, frequency = 12)

  #Simple Moving Average
  sma <- ma(TS, order = 12)
  testset$SimpleMovingAverage <- forecast(TS, h=nrow(testset), level=99)$mean

  #Simple exponential smoothing Holts Winter: Level Only
  simpExpSmooth <- hw(TS, initial = "optimal", beta=NULL, gamma=NULL, level=99) # h is the no. of periods to forecast
  testset$ExponentialSmoothing<-forecast(simpExpSmooth, h=nrow(testset))$mean

  # Double Exponential smoothing Holts Winter: Level and Trend components
  doubleExpSmooth <- hw(TS, initial = "optimal", h=nrow(testset), gamma=NULL)
  testset$DoubleExponentialSmoothing <- forecast(doubleExpSmooth, h=nrow(testset))$mean

  #ETS
  ets <- ets(TS,model="ZZZ")
  testset$ETS <- forecast(ets, h=nrow(testset))$mean

  testset <- subset(testset, select = c(Account, BusinessProcess, Entity, LineItemDetail, Product, Scenario, Time, TimeDataView, Finance_Value, SimpleMovingAverage, ExponentialSmoothing, DoubleExponentialSmoothing , ETS))
  rownames(testset) <- c()
  return(testset)
}
  
predictions <- data.frame(matrix(ncol = 13, nrow = 0))
combo <- unique(data[,c("Entity","Product")])
for(i in seq(1,nrow(combo))){
  temp <- data[data$Entity == combo[i,1] & data$Product == combo[i,2],]
  temp <- temp[order(temp$Time),]
  pred <- as.matrix(train_test(temp))
  predictions <<- rbind(predictions,pred)
  }
'
					,@input_data_1 = N'SELECT * FROM DemoCarSalesData;'
					,@input_data_1_name = N'data'
					,@output_data_1_name = N'predictions'
--WITH result SETS (("Time" Date, "Entity" NVARCHAR(100), "Product" NVARCHAR(100), "Finance_Value" FLOAT, "Prediction" FLOAT ));
SELECT * FROM prediction_data_ts;
END;
GO
Exec Predict_n_EvaluateTS

/* Step 7. Calculate accuracy of the model*/
CREATE OR ALTER PROCEDURE calc_error
AS
BEGIN
TRUNCATE TABLE error;			
INSERT INTO error("Model Name", "MAPE", "MAE", "RMSE")
			EXEC sp_execute_external_script
								@language = N'R'
								,@script = N'
calc_mape <- function(actual,pred){
	return(mean(rowMeans(abs((actual-pred)/actual) * 100)))}
calc_mae <- function(actual,pred) {
	return(mean(rowMeans(abs(actual-pred))))}
calc_rmse <- function(actual,pred) {
	return(sqrt(mean((actual-pred)^2)))}

modelname <- c("Linear Regression", "Decision Tree","Conditional Tree", "Random Forest", "XGBoost")
actual <- data.frame(avp$Finance_Value)
mape <- c(calc_mape(actual,avp$LinearRegression), calc_mape(actual,avp$DecisionTree), calc_mape(actual,avp$ConditionalTree), calc_mape(actual,avp$RandomForest), calc_mape(actual,avp$XGBoost))
mae <- c(calc_mae(actual,avp$LinearRegression),calc_mae(actual,avp$DecisionTree), calc_mae(actual,avp$ConditionalTree), calc_mae(actual,avp$RandomForest), calc_mae(actual,avp$XGBoost))
rmse <- c(calc_rmse(actual,avp$LinearRegression), calc_rmse(actual,avp$DecisionTree), calc_rmse(actual,avp$ConditionalTree), calc_rmse(actual,avp$RandomForest), calc_rmse(actual,avp$XGBoost))
OutputDataSet <- data.frame(modelname, mape, mae, rmse)'
			,@input_data_1 = N'Select * FROM prediction_data'
			,@input_data_1_name = N'avp'
SELECT * FROM error ORDER BY MAPE;
END;
GO
calc_error

CREATE OR ALTER PROCEDURE calc_error_ts
AS
BEGIN
TRUNCATE TABLE error_ts;			
INSERT INTO error_ts("Model Name", "MAPE", "MAE", "RMSE")
			EXEC sp_execute_external_script
								@language = N'R'
								,@script = N'
calc_mape <- function(actual,pred){
	return(mean(rowMeans(abs((actual-pred)/actual) * 100)))}
calc_mae <- function(actual,pred) {
	return(mean(rowMeans(abs(actual-pred))))}
calc_rmse <- function(actual,pred) {
	return(sqrt(mean((actual-pred)^2)))}

modelname <- c( "Simple Moving Average", "Exponential Smoothing", "Double Exponential Smoothing", "ETS")
actual <- data.frame(avp$Finance_Value)
mape <- c(calc_mape(actual,avp$SimpleMovingAverage),calc_mape(actual,avp$ExponentialSmoothing),calc_mape(actual,avp$DoubleExponentialSmoothing),calc_mape(actual,avp$ETS))
mae <- c(calc_mae(actual,avp$SimpleMovingAverage),calc_mae(actual,avp$ExponentialSmoothing),calc_mae(actual,avp$DoubleExponentialSmoothing),calc_mae(actual,avp$ETS))
rmse <- c(calc_rmse(actual,avp$SimpleMovingAverage),calc_rmse(actual,avp$ExponentialSmoothing),calc_rmse(actual,avp$DoubleExponentialSmoothing),calc_rmse(actual,avp$ETS))
OutputDataSet <- data.frame(modelname, mape, mae, rmse)'
			,@input_data_1 = N'Select * FROM prediction_data_ts'
			,@input_data_1_name = N'avp'
--WITH result SETS (("MAPE" FLOAT));
SELECT * FROM error_ts ORDER BY MAPE;
END;
GO
calc_error_ts
--CREATE OR ALTER PROCEDURE calc_error_ts
--AS
--BEGIN
--TRUNCATE TABLE error_ts;			
--INSERT INTO error_ts("Model Name", "MAPE", "MAE", "RMSE")
--			EXEC sp_execute_external_script
--								@language = N'R'
--								,@script = N'
--calc_mape <- function(actual,pred){
--	return(mean(rowMeans(abs((actual-pred)/actual) * 100)))}
--calc_mae <- function(actual,pred) {
--	return(mean(rowMeans(abs(actual-pred))))}
--calc_rmse <- function(actual,pred) {
--	return(sqrt(mean((actual-pred)^2)))}

--modelname <- c( "Simple Moving Average", "Exponential Smoothing", "Double Exponential Smoothing", "ETS", "Linear Regression", "Decision Tree","Conditional Tree", "Random Forest", "XGBoost")
--actual <- data.frame(avp$Finance_Value)
--mape <- c(calc_mape(actual,avp$SimpleMovingAverage),calc_mape(actual,avp$ExponentialSmoothing),calc_mape(actual,avp$DoubleExponentialSmoothing),calc_mape(actual,avp$ETS),calc_mape(actual,avp$LinearRegression), calc_mape(actual,avp$DecisionTree), calc_mape(actual,avp$ConditionalTree), calc_mape(actual,avp$RandomForest), calc_mape(actual,avp$XGBoost))
--mae <- c(calc_mae(actual,avp$SimpleMovingAverage),calc_mae(actual,avp$ExponentialSmoothing),calc_mae(actual,avp$DoubleExponentialSmoothing),calc_mae(actual,avp$ETS),calc_mae(actual,avp$LinearRegression),calc_mae(actual,avp$DecisionTree), calc_mae(actual,avp$ConditionalTree), calc_mae(actual,avp$RandomForest), calc_mae(actual,avp$XGBoost))
--rmse <- c(calc_rmse(actual,avp$SimpleMovingAverage),calc_rmse(actual,avp$ExponentialSmoothing),calc_rmse(actual,avp$DoubleExponentialSmoothing),calc_rmse(actual,avp$ETS),calc_rmse(actual,avp$LinearRegression), calc_rmse(actual,avp$DecisionTree), calc_rmse(actual,avp$ConditionalTree), calc_rmse(actual,avp$RandomForest), calc_rmse(actual,avp$XGBoost))
--OutputDataSet <- data.frame(modelname, mape, mae, rmse)'
--			,@input_data_1 = N'Select * FROM prediction_data_ts'
--			,@input_data_1_name = N'avp'
----WITH result SETS (("MAPE" FLOAT));
--SELECT * FROM error_ts ORDER BY MAPE;
--END;
--GO



CREATE OR ALTER PROCEDURE forecast @modelname NVARCHAR(50), @forecast_period INT
AS
BEGIN
TRUNCATE TABLE forecast_data;
INSERT INTO forecast_data("Account","BusinessProcess","Entity", "LineItemDetail","Product", "Scenario","Time", "TimeDataView", "LinearRegression", "RandomForest", "XGBoost")
			EXEC sp_execute_external_script
								@language = N'R'
								,@script = N'
library(DataCombine)
library(randomForest)
library(ridge)
library(glmnet)
library(gbm)
library(rpart)
library(party)
library(caret)
library(xgboost)
library(forecast)
library(tseries)
train <- data.frame(train)

train_test_forecast <- function(data){
  forecast_period <- 12
  modelname <- "Random Forest"
  data$year<-as.numeric(format(as.Date(data$Time), "%Y"))
  data$week_day<-as.numeric(format(as.Date(data$Time), "%w"))
  data$month<-as.numeric(format(as.Date(data$Time), "%m"))
  data$week<-as.numeric(format(as.Date(data$Time), "%W"))
  data$day_of_month <- as.numeric(format(as.Date(data$Time), "%d"))
  
  
  data_seas<-t(matrix(data=data$Finance_Value))
  data$seas<-colMeans(data_seas,na.rm=T)

  #forecast fourier series
  data_msts <- msts(data$Finance_Value, seasonal.periods = c(12))
  K <- 2 #2 - K must be not be greater than period/2
  Fourier <- data.frame(fourier(data_msts, K = K))
  colnames(Fourier) = c("FourierS1", "FourierC1", "FourierS2", "FourierC2")
  data <- data.frame(data,Fourier)


  regress_form <- "Finance_Value~"
  for(i in seq(7:12)){
    data <- slide(data, "Finance_Value" , NewVar = paste0("Lag_",i), slideBy = -i)  # create lag variables
	regress_form <- ifelse(i==1,paste(regress_form,"Lag_",i,sep=""),paste(regress_form,"+","Lag_",i,sep=""))
    # data <- slide(data, "Beer", NewVar = ld, slideBy = i)  # create lead1 variable
  }
  #regress_form <- paste(regress_form,"+year+week_day+month+week+day_of_month",sep="")
  #Remove NAs from the dataset
  clean_data <- data[complete.cases(data), ]
  
  #Create data for forecast
  last_date <- as.Date(data[nrow(data),"Time"])
  forecast_start_date <- seq(last_date, by="month", length.out = 2)[2]
  forecasttest <- data.frame(Time = seq(forecast_start_date, by = "month", length.out = forecast_period)) #forecast_period

  forecasttest$Account <- clean_data[1,"Account"]
  forecasttest$BusinessProcess <- clean_data[1,"BusinessProcess"]
  forecasttest$Entity <- clean_data[1,"Entity"]
  forecasttest$LineItemDetail <- clean_data[1,"LineItemDetail"]
  forecasttest$Product <- clean_data[1,"Product"]
  forecasttest$Scenario <- clean_data[1,"Scenario"]
  forecasttest$TimeDataView <- clean_data[1,"TimeDataView"]
  forecasttest$year <- as.numeric(format(as.Date(forecasttest$Time), "%Y"))
  forecasttest$week_day <- as.numeric(format(as.Date(forecasttest$Time), "%w"))
  forecasttest$month <- as.numeric(format(as.Date(forecasttest$Time), "%m"))
  forecasttest$week <- as.numeric(format(as.Date(forecasttest$Time), "%W"))
  forecasttest$day_of_month <- as.numeric(format(as.Date(forecasttest$Time), "%d"))
  
  Fourier <- data.frame(fourier(data_msts, K = K, h=forecast_period)) #forecast_period
  colnames(Fourier) = c("FourierS1", "FourierC1", "FourierS2", "FourierC2")
  forecasttest <- data.frame(forecasttest,Fourier)
  seas_fit <- auto.arima(data$seas)
  forecasttest$seas <- forecast(seas_fit,12)$mean #forecast_period
 
  lag1_fit<-auto.arima(clean_data$Lag_7)
  lag2_fit<-auto.arima(clean_data$Lag_8)
  lag3_fit<-auto.arima(clean_data$Lag_9)
  lag4_fit<-auto.arima(clean_data$Lag_10)
  lag5_fit<-auto.arima(clean_data$Lag_11)
  lag6_fit<-auto.arima(clean_data$Lag_12)

  forecasttest$Lag_1<-as.matrix(as.numeric(forecast(lag1_fit,12)$mean))
  forecasttest$Lag_2<-as.matrix(as.numeric(forecast(lag2_fit,12)$mean))
  forecasttest$Lag_3<-as.matrix(as.numeric(forecast(lag3_fit,12)$mean))
  forecasttest$Lag_4<-as.matrix(as.numeric(forecast(lag4_fit,12)$mean))
  forecasttest$Lag_5<-as.matrix(as.numeric(forecast(lag5_fit,12)$mean))
  forecasttest$Lag_6<-as.matrix(as.numeric(forecast(lag6_fit,12)$mean))

  x_train <- model.matrix(~Lag_7+Lag_8+Lag_9+Lag_10+Lag_11+Lag_12,clean_data) #Removes Target
  y_train <- as.matrix(subset(clean_data,select = c(Finance_Value))) #Only Target
  x_test <- model.matrix(~Lag_7+Lag_8+Lag_9+Lag_10+Lag_11+Lag_12,forecasttest)

  #Rx Linear Regresssion - Working
  lr_model <- rxLinMod(regress_form, data = clean_data);
  forecasttest$LinearRegression <- as.matrix(rxPredict(lr_model, forecasttest))

  #Regular Random Forest - Working
  rf <- randomForest(as.formula(regress_form), data = data.frame(clean_data), ntree = 100, nodesize = 3, importance = TRUE)
  forecasttest$RandomForest <- as.matrix(predict(rf,forecasttest))

  #Regular XGBoost
  xgb <- xgboost(data= xgb.DMatrix(data = data.matrix(clean_data), label = clean_data$Finance_Value), nfold = 5,nrounds = 5, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", max_depth = 3, nthread = 4)
  forecasttest$XGBoost <- predict(xgb,data.matrix(forecasttest))

  forecasttest <- subset(forecasttest, select = c(Account, BusinessProcess, Entity, LineItemDetail, Product, Scenario, Time, TimeDataView,LinearRegression,RandomForest,XGBoost))
  rownames(forecasttest) <- c()
  return(forecasttest)

} 

predictions <- data.frame(matrix(ncol = 11, nrow = 0))
combo <- unique(train[,c("Entity", "Product")])
for(i in seq(1,nrow(combo))){
  temp <- train[train$Entity == combo[i,1] & train$Product == combo[i,2],]
  temp <- temp[order(temp$Time),]
  pred <- as.matrix(train_test_forecast(temp))
  predictions <<- rbind(predictions,pred)
    
}'
			,@input_data_1 = N'SELECT * FROM DemoCarSalesData'
			,@input_data_1_name = N'train'
			,@output_data_1_name = N'predictions'
			,@params = N'@modelname NVARCHAR(50), @forecast_period INT'
			,@modelname = @modelname
			,@forecast_period = @forecast_period
			
--WITH result SETS (("MAPE" FLOAT));
END;
GO
EXEC forecast @modelname = 'Random Forest', @forecast_period = 12;
select * from forecast_data


  forecasttest$Lag_1<-as.matrix(as.numeric(forecast(lag1_fit,12)$mean))
  forecasttest$Lag_2<-as.matrix(as.numeric(forecast(lag2_fit,12)$mean))
  forecasttest$Lag_3<-as.matrix(as.numeric(forecast(lag3_fit,12)$mean))
  forecasttest$Lag_4<-as.matrix(as.numeric(forecast(lag4_fit,12)$mean))
  forecasttest$Lag_5<-as.matrix(as.numeric(forecast(lag5_fit,12)$mean))
  forecasttest$Lag_6<-as.matrix(as.numeric(forecast(lag6_fit,12)$mean))


  x_train <- model.matrix(~Lag_1+Lag_2+Lag_3+Lag_4+Lag_5+Lag_6+seas+FourierS1+FourierC1+FourierS2+FourierC2,clean_data) #Removes Target
  y_train <- as.matrix(subset(clean_data,select = c(Finance_Value))) #Only Target
  x_test <- model.matrix(~Lag_1+Lag_2+Lag_3+Lag_4+Lag_5+Lag_6+seas+FourierS1+FourierC1+FourierS2+FourierC2,forecasttest)

  model_list <- list("Linear Regression" = lm(regress_form, data = clean_data),
				"Decision Tree" = rpart(regress_form, method="anova",data= clean_data), 
				#"Conditional Tree" = ctree(as.formula(regress_form), data=clean_data), 
				"Random Forest" = randomForest(as.formula(regress_form), data = data.frame(clean_data), ntree = 50, nodesize = 3, importance = TRUE),
				"XG Boost" = xgboost(data= xgb.DMatrix(data = data.matrix(x_train), label = y_train), nfold = 5,nrounds = 5, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", max_depth = 3, nthread = 4)
				)
  predict_list <- list("Linear Regression" = as.matrix(predict(model_list[["Linear Regression"]], forecasttest)),
				"Decision Tree" = as.matrix(predict(model_list[["Decision Tree"]],forecasttest)),
				#"Conditional Tree" = as.matrix(predict(model_list[["Conditional Tree"]],forecasttest)),
				"Random Forest" = as.matrix(predict(model_list[["Random Forest"]],forecasttest)),
				"XG Boost" = as.matrix(predict(model_list[["XG Boost"]],data.matrix(x_test)))
				)

  forecasttest$Forecast <- predict_list[[modelname]]

  forecasttest <- subset(forecasttest, select = c(Account, BusinessProcess,Entity, LineItemDetail, Product, Scenario, Time, TimeDataView,Forecast))
  rownames(forecasttest) <- c()
  #return(forecasttest)   