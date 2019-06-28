USE demosalespy;

DROP TABLE IF EXISTS prediction_data_py_svm;
CREATE TABLE prediction_data_py_svm(
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
				,"Linear_SVR" FLOAT NOT NULL
				,"Poly_SVR" FLOAT NOT NULL
				,"RBF_SVR" FLOAT NOT NULL
				);

CREATE OR ALTER PROCEDURE Predict_n_Evaluate
AS
BEGIN
TRUNCATE TABLE prediction_data_py_svm;
INSERT INTO prediction_data_py_svm("Account"
				,"BusinessProcess" 
				,"Entity"
				,"LineItemDetail"
				,"Product"
				,"Scenario" 
				,"Time" 
				,"TimeDataView" 
				,"Finance_Value" 
				,"LinearRegression" 
				,"Linear_SVR"
				,"Poly_SVR"
				,"RBF_SVR"
				)
EXEC sp_execute_external_script
					@language = N'Python'
					,@script = N'

import pandas as pd
import numpy as np
from datetime import datetime
from sklearn.svm import SVR #Import SVM SVR package 
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error #Prediction error metrics
from sklearn.metrics import mean_absolute_error

#Avoid warning displays
import warnings
warnings.filterwarnings("ignore")

def Create_TimeIndex(df):
    df["Time"]=pd.to_datetime(df["Time"])
    x=(df["Time"].dt.year-2017)*12+df["Time"].dt.month
    return x

def train_test(data):
    
    data["year"]=data["Time"].dt.year
    data["week_day"]=data["Time"].dt.weekday+1
    data["week"]=data["Time"].dt.week
    data["month"]=data["Time"].dt.month
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
    
    #Linear Regression
    linreg=LinearRegression().fit(x_train, y_train)
    testset["LinearRegression"]=linreg.predict(x_test)
    
    #Support Vector Machines
    
    #Linear SV Regressor
    linear_svr=SVR(kernel="linear", C=100, epsilon=0.01, gamma="auto")
    l_svr=linear_svr.fit(x_train, y_train)
    testset["Linear_SVR"]=l_svr.predict(x_test)
    
    #Polynomial SV Regressor
    poly_svr=SVR(kernel="poly", C=100, gamma="auto", degree=3, epsilon=0.01, coef0=1)
    p_svr=poly_svr.fit(x_train, y_train)
    testset["Poly_SVR"]=p_svr.predict(x_test)
    testset.loc[testset["Poly_SVR"]<0,"Poly_SVR"]=0
    
    #Radial Basis Fucntion(RBF) SV Regressor
    rbf_svr=SVR(kernel="rbf", C=100, gamma=0.1, epsilon=0.01 )
    r_svr=rbf_svr.fit(x_train, y_train)
    testset["RBF_SVR"]=r_svr.predict(x_test)
    
    returnset=testset[["Account", "BusinessProcess", "Entity", "LineItemDetail", "Product", "Scenario", "Time","TimeIndex", "TimeDataView", "Finance_Value", "Linear_SVR", "LinearRegression", "Poly_SVR", "RBF_SVR"]]

    return returnset
    
predictions=pd.DataFrame([])
combo=pd.DataFrame([])
#Finding unique combination of Entity and Product
combine=maindata["Entity"]+"_"+maindata["Product"]
new=pd.unique(combine)
new=pd.Series(new)
new=new.str.split("_",expand=True)
combo["Entity"]=new[0]
combo["Product"]=new[1]

#Simulate Training. Consolidate all predictions in a dataframe 
#for i in range(0,len(combo)):
temp=maindata[(maindata["Entity"]=="LE6") & (maindata["Product"]=="A500")] #combo.iloc[i,0]) & (maindata["Product"]==combo.iloc[i,1])] #
temp=temp.sort_values(by="Time", ascending=True)
pred=train_test(temp) #contains all test dataset with model predicted values in each column
predictions=predictions.append(pred) #Append predictions on test data for every combination

print("first look at the dataset")
print(temp.head())
pred=train_test(temp) #contains all test dataset with model predicted values in each column
predictions=predictions.append(pred) #Append predictions on test data for every combination
print("These are the predictions")
print(predictions.head())
'
,@input_data_1 = N'SELECT * FROM Data_py'
,@input_data_1_name = N'maindata'
,@output_data_1_name = N'predictions'
SELECT * FROM prediction_data_py_svm;
END;
Go
EXEC Predict_n_Evaluate

use demosalespy;
select * from prediction_data_py;
select * from Data_py where Entity = 'LE6' and Product = 'A500';
Go
