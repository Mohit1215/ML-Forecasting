/* Step 1. Set up Schema*/
USE DemoSales;

DROP TABLE IF EXISTS prediction_data_ti, error_ti, forecast_data_ti;
CREATE TABLE prediction_data_ti(
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
DROP TABLE IF EXISTS error_ti;
CREATE TABLE error_ti (
				"Model Name" VARCHAR(100) NOT NULL,
				"MAPE" FLOAT NOT NULL,
				"MAE" float  not null
				,"RMSE" FLOAT NOT NULL);

CREATE TABLE forecast_data_ti(
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

/* Process input dataset before splitting*/
CREATE OR ALTER PROCEDURE Predict_n_Evaluate
AS
BEGIN
TRUNCATE TABLE prediction_data_ti;
INSERT INTO prediction_data_ti("Account"
				,"BusinessProcess" 
				,"Entity"
				,"LineItemDetail"
				,"Product"
				,"Scenario" 
				,"Time" 
				,"TimeDataView" 
				,"Finance_Value" 
				,"LinearRegression" 
				,"DecisionTree"
				,"ConditionalTree"
				,"RandomForest" 
				)
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

create_TimeIndex <- function(x){
 
  #return(as.numeric(difftime(as.Date(x),as.Date("2017-01-01"),units="months")))
  return(((as.numeric(format(as.Date(x), "%Y"))-as.numeric(format(as.Date("2017-01-01"), "%Y")))*12) + as.numeric(format(as.Date(x), "%m")))
}

train_test <- function(data){

  data$year<-as.numeric(format(as.Date(data$Time), "%Y"))
  data$week_day<-as.numeric(format(as.Date(data$Time), "%w"))
  data$month<-as.factor(format(as.Date(data$Time), "%m"))
  data$week<-as.numeric(format(as.Date(data$Time), "%W"))
  start_date <- as.Date(data[1,"Time"])
  data$TimeIndex <- sapply(data$Time, FUN = create_TimeIndex)

  regress_form <- Finance_Value~month+TimeIndex+week_day+week
  
  #Split train and validation sets
  #index <- 1:(floor(nrow(data)*0.8))
  index <- 1: (nrow(data) - 6)
  testset <- data[-index,]
  new_testset <-subset(testset, select = -c(Finance_Value))
  trainset <- data[index,]
  x_train <- data.matrix(subset(trainset,select = c(month,TimeIndex))) # Removes Target
  y_train <- as.double(as.matrix(trainset[, "Finance_Value"])) # Only Target
  x_test <- data.matrix(subset(testset,select = c(month,TimeIndex)))
  
  #Linear Regression
  lm_fit <- lm(formula = as.formula(regress_form), data = trainset)
  testset$LinearRegression <- predict(lm_fit, newdata=new_testset, type = "response")
  
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

  #Random Forest
  rf_model <- randomForest(as.formula(regress_form), data = data.frame(trainset),
                         ntree = 1000, mtry = 3, nodesize = 3, importance = TRUE)
  testset$RandomForest <- predict(rf_model,new_testset)
  

  
  testset <- subset(testset, select = c(Account, BusinessProcess, Entity, LineItemDetail, Product, Scenario, Time, TimeDataView, Finance_Value,LinearRegression, DecisionTree,ConditionalTree,RandomForest))
  rownames(testset) <- c()
  return(testset)
}
  
predictions <- data.frame(matrix(ncol = 13, nrow = 0))
#combo <- unique(data[,c("Store","Dept")])
#for(i in seq(1,nrow(combo))){
  #temp <- data[data$Store == combo[i,1] & data$Dept == combo[i,2],]
  temp <- data[data$Entity == "LE6" & data$Product == "A100",]
  temp <- temp[order(temp$Time),]
  print(temp)
  pred <- as.matrix(train_test(temp))
  predictions <<- rbind(predictions,pred)'
/*}'*/
					,@input_data_1 = N'SELECT * FROM Data;'
					,@input_data_1_name = N'data'
					,@output_data_1_name = N'predictions'
--WITH result SETS (("Time" Date, "Entity" NVARCHAR(100), "Product" NVARCHAR(100), "Finance_Value" FLOAT, "Prediction" FLOAT ));
SELECT * FROM prediction_data_ti;
END;
GO
EXEC Predict_n_Evaluate


/* Step 7. Calculate accuracy of the model*/
CREATE OR ALTER PROCEDURE calc_error
AS
BEGIN
TRUNCATE TABLE error_ti;			
INSERT INTO error_ti("Model Name", "MAPE", "MAE", "RMSE")
			EXEC sp_execute_external_script
								@language = N'R'
								,@script = N'
calc_mape <- function(actual,pred){
	return(mean(rowMeans(abs((actual-pred)/actual) * 100)))}
calc_mae <- function(actual,pred) {
	return(mean(rowMeans(abs(actual-pred))))}
calc_rmse <- function(actual,pred) {
	return(sqrt(mean((actual-pred)^2)))}

modelname <- c( "Linear Regression", "Decision Tree", "Conditional Tree", "Random Forest")
actual <- data.frame(avp$Finance_Value)
mape <- c(calc_mape(actual,avp$LinearRegression), calc_mape(actual,avp$DecisionTree),calc_mape(actual,avp$ConditionalTree), calc_mape(actual,avp$RandomForest))
mae <- c(calc_mae(actual,avp$LinearRegression), calc_mae(actual,avp$DecisionTree),calc_mae(actual,avp$ConditionalTree), calc_mae(actual,avp$RandomForest))
rmse <- c(calc_rmse(actual,avp$LinearRegression), calc_rmse(actual,avp$DecisionTree), calc_rmse(actual,avp$ConditionalTree),calc_rmse(actual,avp$RandomForest))
OutputDataSet <- data.frame(modelname, mape, mae, rmse)'
			,@input_data_1 = N'Select * FROM prediction_data_ti'
			,@input_data_1_name = N'avp'
--WITH result SETS (("MAPE" FLOAT));
SELECT * FROM error_ti ORDER BY MAPE;
END;
GO
calc_error









USE DemoSales;


CREATE TABLE forecast_data_ti1_rf(
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





CREATE OR ALTER PROCEDURE forecast @modelname NVARCHAR(50), @forecast_period INT
AS
BEGIN
TRUNCATE TABLE forecast_data_ti1_rf;
INSERT INTO forecast_data_ti1_rf("Account"
				,"BusinessProcess" 
				,"Entity"
				,"LineItemDetail"
				,"Product"
				,"Scenario"
				,"Time"
				,"TimeDataView"
				,"Forecast")
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
train <- data.frame(train)

create_TimeIndex <- function(x){

  #return(as.numeric(difftime(as.Date(x),as.Date("2017-01-01"),units="weeks")))
   return(((as.numeric(format(as.Date(x), "%Y"))-as.numeric(format(as.Date("2017-01-01"), "%Y")))*12) + as.numeric(format(as.Date(x), "%m")))

  }

train_test_forecast <- function(data){
  data$year<-as.numeric(format(as.Date(data$Time), "%Y"))
  data$week_day<-as.numeric(format(as.Date(data$Time), "%w"))
  data$month<-as.factor(format(as.Date(data$Time), "%m"))
  data$week<-as.numeric(format(as.Date(data$Time), "%W"))
  start_date <- as.Date(data[1,"Time"])
  data$TimeIndex <- sapply(data$Time, FUN = create_TimeIndex)

  regress_form <- Finance_Value ~ month + TimeIndex 

  clean_data <- data[complete.cases(data), ]
  #Create Forecast dataset
  last_date <- as.Date(data[nrow(data),"Time"])
  forecast_start_date <- seq(last_date, by="month", length.out = 2)[2]
  forecasttest <- data.frame(Time = seq(forecast_start_date, by = "month", length.out = 60)) #forecast_period
  forecasttest$year <- as.numeric(format(as.Date(forecasttest$Time), "%Y"))
  forecasttest$week_day <- as.numeric(format(as.Date(forecasttest$Time), "%w"))
  forecasttest$month <- as.factor(format(as.Date(forecasttest$Time), "%m"))
  forecasttest$week <- as.numeric(format(as.Date(forecasttest$Time), "%W"))
  forecasttest$TimeIndex <- sapply(forecasttest$Time, FUN = create_TimeIndex)


  forecasttest$Account <- clean_data[1,"Account"]
  forecasttest$BusinessProcess <- clean_data[1,"BusinessProcess"]
  forecasttest$Entity <- clean_data[1,"Entity"]
  forecasttest$LineItemDetail <- clean_data[1,"LineItemDetail"]
  forecasttest$Product <- clean_data[1,"Product"]
  forecasttest$Scenario <- clean_data[1,"Scenario"]
  forecasttest$TimeDataView <- clean_data[1,"TimeDataView"]


  ########
  # To implement fetching data from another table from the sql server using RxSqL and DBI connection (odbc) 

 # library(DBI)
	#conn <-
	#		"Driver=SQL Server; 
	#		Server=MOHIT-PARKHI-DE\\SQLEXPRESS;
	#		Database=wal; 
	#		Trusted_Connection=True"
	#query <- "select * from futurevalues;"

	#sampledata <- RxSqlServerData(connectionString = conn ,sqlQuery = query)
	#sampledata <- rxDataStep(sampledata)
	#sampledata <- as.data.frame(sampledata)
  ########
  
  #print(head(sampledata$Temperature))
  
  #forecasttest$Store <- sampledata[1:52,"Store"]
  #forecasttest$Dept <- sampledata[1:52,"Dept"]
  #forecasttest$date <- sampledata[1:52,"date"]
  #forecasttest$Temperature <- sampledata[1:52,"Temperature"]
  
  #print(head(forecasttest$Temperature))

  #forecasttest$Fuel_Price <- sampledata[1:52,"Fuel_Price"]
  #forecasttest$MarkDown1 <- sampledata[1:52,"MarkDown1"]
  #forecasttest$MarkDown2 <- sampledata[1:52,"MarkDown2"]
  #forecasttest$MarkDown3 <- sampledata[1:52,"MarkDown3"]
  #forecasttest$MarkDown4 <- sampledata[1:52,"MarkDown4"]
  #forecasttest$MarkDown5 <- sampledata[1:52,"MarkDown5"]
  #forecasttest$CPI <- sampledata[1:52,"CPI"]
  #forecasttest$Unemployment <- sampledata[1:52,"Unemployment"]
  #forecasttest$IsHoliday <- sampledata[1:52,"IsHoliday"]




  

  #x_train <- data.matrix(subset(data,select = c(week,TimeIndex,Temperature, Fuel_Price, MarkDown1, MarkDown2, MarkDown3, MarkDown4,
						#	 MarkDown5))) # Removes Target
  #y_train <- as.double(as.matrix(data[, "Weekly_Sales"])) # Only Target
  #x_test <- data.matrix(subset(forecasttest,select = c(week,TimeIndex,Temperature, Fuel_Price, MarkDown1, MarkDown2, MarkDown3, MarkDown4,
						#	 MarkDown5)))
  
  model_list <- list("Linear Regression" = lm(formula = as.formula(regress_form), data = data),
				#"Ridge Regression" = cv.glmnet(data.matrix(x_train),y_train,alpha=0),
				#"Lasso Regression" = cv.glmnet(data.matrix(x_train),y_train,alpha=1),
				"Decision Tree" = rpart(regress_form, control = rpart.control(cp = 0.01, minsplit = 5, maxdepth = 4), data= data), 
				#"Conditional Tree" = ctree(as.formula(regress_form), data=data), 
				"Random Forest" = randomForest(as.formula(regress_form), data = data.frame(data), ntree = 100, mtry = 3, nodesize = 3, importance = TRUE)
				#"XG Boost" = xgboost(data= xgb.DMatrix(data = data.matrix(x_train), label = y_train), nfold = 5,nrounds = 5, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", max_depth = 3, nthread = 4)
				)
  predict_list <- list(#"Ridge Regression" = as.matrix(predict(model_list[["Ridge Regression"]], newx=data.matrix(x_test), s = "lambda.min")),
				#"Lasso Regression" = as.matrix(predict(model_list[["Lasso Regression"]], newx=data.matrix(x_test), s = "lambda.min")),
				"Linear Regression" = as.matrix(predict(model_list[["Linear Regression"]], forecasttest)),
				"Decision Tree" = as.matrix(predict(model_list[["Decision Tree"]],forecasttest)),
				#"Conditional Tree" = as.matrix(predict(model_list[["Conditional Tree"]],forecasttest)),
				"Random Forest" = as.matrix(predict(model_list[["Random Forest"]],forecasttest))
				#"XG Boost" = as.matrix(predict(model_list[["XG Boost"]],data.matrix(x_test)))
				)

  forecasttest$Forecast <- predict_list[[modelname]]

  forecasttest <- subset(forecasttest, select = c(Account, BusinessProcess, Entity, LineItemDetail, Product, Scenario, Time, TimeDataView, Forecast))
  rownames(forecasttest) <- c()
  return(forecasttest)   

} 

predictions <- data.frame(matrix(ncol = 9 , nrow = 0))
#combo <- unique(train[,c("Store", "Dept")])
#for(i in seq(1,nrow(combo))){
  #temp <- train[train$Store == combo[i,1] & train$Dept == combo[i,2],]
  temp <- train[train$Entity == "LE6" & train$Product == "A100", ]
  temp <- temp[order(temp$Time),]
  pred <- as.matrix(train_test_forecast(temp))
  predictions <<- rbind(predictions,pred)'
    

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
select * from forecast_data_ti1_rf