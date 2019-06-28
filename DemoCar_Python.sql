USE demosalespy;

DROP TABLE IF EXISTS prediction_data_py, error_ti, forecast_data_ti;
CREATE TABLE prediction_data_py(
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
				,"RidgeRegression" FLOAT NOT NULL
				,"LassoRegression" FLOAT NOT NULL
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


use demosalespy;
CREATE OR ALTER PROCEDURE Predict_n_Evaluate
AS
BEGIN
TRUNCATE TABLE prediction_data_py;
INSERT INTO prediction_data_py("Account"
				,"BusinessProcess" 
				,"Entity"
				,"LineItemDetail"
				,"Product"
				,"Scenario" 
				,"Time" 
				,"TimeDataView" 
				,"Finance_Value" 
				,"LinearRegression" 
				,"RidgeRegression"
				,"LassoRegression"
				,"RandomForest" 
				)
EXEC sp_execute_external_script
					@language = N'Python'
					,@script = N'

import pandas as pd
import numpy as np
from datetime import datetime
from sklearn.linear_model import LinearRegression
from sklearn.linear_model import LassoCV, Ridge, RidgeCV, ElasticNet
from sklearn.ensemble import RandomForestRegressor

from sklearn.svm import SVR #Import SVM SVR package 
#from statsmodels.tsa.api import SimpleExpSmoothing
#from statsmodels.tsa.api import Holt #Holtz Winter exponential smoothing
#from statsmodels.tsa.api import ExponentialSmoothing
#import statsmodels.formula.api as sm
#from sklearn.metrics import mean_squared_error #Prediction error metrics
#from sklearn.metrics import mean_absolute_error

#Avoid warning displays
#import warnings
#warnings.filterwarnings("ignore")

"""
def Create_TimeIndex(df):
    #df["date"]=pd.to_datetime(df["date"])
    x=(pd.DatetimeIndex(df["date"]).month-2017)*12+pd.DatetimeIndex(df["date"]).month
    return x



data["date"] = pd.to_datetime(data["Time"])
#print(data["Time"].head())
#print(data["date"].head())
#print(type(data))
#data["date"] = datetime.strptime(data["Time"][0],"%d-%m-%Y")

#data["date"] = data["Time"].strftime("%m-%d-%Y")
#data["month"] = pd.DatetimeIndex(data["Time"]).month
#print(data["Time"][0])
#print(data["month"])
#print(data["date"])

data["month"] = pd.DatetimeIndex(data["date"]).month
data["weekday"] = pd.DatetimeIndex(data["date"]).weekday
data["week"] = pd.DatetimeIndex(data["date"]).week
data["year"] = pd.DatetimeIndex(data["date"]).year

#print(data["month"].head())
#print("weekday is:")
#print(data["weekday"].head())
#print(data["week"].head())
#print(data["year"].head())


dummies=pd.get_dummies(data["month"], prefix="month") #Creating dummies for month field. Treating it as categorical.
data=data.join(dummies)
start_date=data.iloc[:1,6]
    
data["TimeIndex"]=Create_TimeIndex(data)

#Split in train-test
index=list(range(0,(len(data)-6)))
testset=data.iloc[len(data)-6:,]
new_testset=testset.iloc[:,:8] #Drop Finance column
trainset=data.iloc[index,]
x_train=trainset.iloc[:,13:]
y_train=trainset.loc[:,["Finance_Value"]]
x_test=testset.iloc[:,13:]    
    
#Linear regression
linreg=LinearRegression().fit(x_train, y_train)
testset["LinearRegression"]=linreg.predict(x_test)
    
# Ridge Regression
ridgeReg = Ridge().fit(x_train,y_train)
testset["RidgeRegression"] = ridgeReg.predict(x_test)


 # Lasso Regression
lassoReg = LassoCV(alphas = [1, 0.1, 0.001, 0.0005]).fit(x_train, y_train)
# Try different alphas to examine what the best value is.
testset["LassoRegression"] = lassoReg.predict(x_test)

#Random Forest
rf_model = RandomForestRegressor(n_estimators = 1000, max_depth=3,random_state = 42)
rf=rf_model.fit(x_train,y_train)
testset["RandomForest"]=rf.predict(x_test)
    
returnset=testset[["Account", "BusinessProcess", "Entity", "LineItemDetail", "Product", "Scenario", "Time","TimeIndex", "TimeDataView", "Finance_Value", "LinearRegression", "RidgeRegression","LassoRegression","RandomForest"]]

#return returnset



#print(returnset)






"""

print("start from here")

####################
def Create_TimeIndex(df):
    df["Time"]=pd.to_datetime(df["Time"])
    x=(df["Time"].dt.year-2017)*12+df["Time"].dt.month
    return x
###########


def train_test(data):
    
	"""
	data["year"]=data["Time"].dt.year
    data["week_day"]=data["Time"].dt.weekday+1
    data["week"]=data["Time"].dt.week
    data["month"]=data["Time"].dt.month
	"""
	data["date"] = pd.to_datetime(data["Time"])

	data["month"] = pd.DatetimeIndex(data["date"]).month
	data["weekday"] = pd.DatetimeIndex(data["date"]).weekday
	data["week"] = pd.DatetimeIndex(data["date"]).week
	data["year"] = pd.DatetimeIndex(data["date"]).year


    dummies=pd.get_dummies(data["month"], prefix="month") #Creating dummies for month field. Treating it as categorical.
    data=data.join(dummies)
    start_date=data.iloc[:1,6]
    
    data["TimeIndex"]=Create_TimeIndex(data)
    
    #Split in train-test
    index=list(range(0,(len(data)-6)))
    testset=data.iloc[len(data)-6:,]
    new_testset=testset.iloc[:,:8] #Drop Finance column
    trainset=data.iloc[index,]
    x_train=trainset.iloc[:,13:]
    y_train=trainset.loc[:,["Finance_Value"]]
    x_test=testset.iloc[:,13:]    
    
    #Linear regression
    linreg=LinearRegression().fit(x_train, y_train)
    testset["LinearRegression"]=linreg.predict(x_test)
    
    # Ridge Regression
    ridgeReg = Ridge().fit(x_train,y_train)
    testset["RidgeRegression"] = ridgeReg.predict(x_test)
    
    # Lasso Regression
    lassoReg = LassoCV(alphas = [1, 0.1, 0.001, 0.0005]).fit(x_train, y_train)
    # Try different alphas to examine what the best value is.
    testset["LassoRegression"] = lassoReg.predict(x_test)
    
    #Simple Moving Average
    #testset["SimpleMovingAverage"] = trainset["Finance_Value"].rolling(12).mean().iloc[-1]
    
    #simple Exponential Smoothing
    #fit = SimpleExpSmoothing(np.asarray(trainset["Finance_Value"])).fit(smoothing_level=0.3, optimized=False)
    #testset["SES"] = fit.forecast(len(testset))
    
    #Holt linear
    #fit = Holt(np.asarray(trainset["Finance_Value"])).fit(smoothing_level=0.3, smoothing_slope=0.1)
    #testset["Holt_linear"] = fit.forecast(len(testset))
    
    #Holt Winter ETS
    #fit1 = ExponentialSmoothing(np.asarray(trainset["Finance_Value"]), seasonal_periods= 5, trend= None, seasonal= "mul").fit()
    #testset["Holt_Winter"] = fit1.forecast(len(testset))
    
    #Random Forest
    rf_model = RandomForestRegressor(n_estimators = 1000, max_depth=3,random_state = 42)
    rf=rf_model.fit(x_train,y_train)
    testset["RandomForest"]=rf.predict(x_test)
    
    returnset=testset[["Account", "BusinessProcess", "Entity", "LineItemDetail", "Product", "Scenario", "Time","TimeIndex", "TimeDataView", "Finance_Value", "LinearRegression", "RidgeRegression","LassoRegression","RandomForest"]]

    return returnset


#############################
#Prediction 

predictions=pd.DataFrame([])
combo=pd.DataFrame([])
#Finding unique combination of Entity and Product
combine=maindata["Entity"]+"_"+maindata["Product"]
new=pd.unique(combine)
new=pd.Series(new)
new=new.str.split("_",expand=True)
combo["Entity"]=new[0]
combo["Product"]=new[1]

################################

#Simulate Training. Consolidate all predictions in a dataframe 
for i in range(0,len(combo)):
    temp=maindata[(maindata["Entity"]==combo.iloc[i,0]) & (maindata["Product"]==combo.iloc[i,1])] #=="LE1") & (maindata["Product"]=="A100")] 
    temp=temp.sort_values(by="Time", ascending=True)
    pred=train_test(temp) #contains all test dataset with model predicted values in each column
    predictions=predictions.append(pred) #Append predictions on test data for every combination
###
##########################################################
"""
#Prediction Error calculation
def rmse(targets, predictions):
    return np.sqrt(mean_squared_error(targets, predictions))
##########################################################

"""


'
,@input_data_1 = N'SELECT *  FROM Data_py '
,@input_data_1_name = N'data'
,@output_data_name = N'predictions'
END;
Go
EXEC Predict_n_Evaluate;

use demosalespy;
select * from prediction_data_py;
select * from data where Entity = 'LE6' and Product = 'A500';
Go