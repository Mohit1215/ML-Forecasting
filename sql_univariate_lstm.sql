USE demosalespy;

DROP TABLE IF EXISTS prediction_data_py;
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
import numpy
import matplotlib.pyplot as plt
import pandas
import math
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_squared_error

numpy.random.seed(7)

import pandas as pd
import numpy as np
from datetime import datetime
from sklearn.linear_model import LinearRegression
from sklearn.linear_model import LassoCV, Ridge, RidgeCV, ElasticNet
from sklearn.ensemble import RandomForestRegressor
from statsmodels.tsa.api import SimpleExpSmoothing
from statsmodels.tsa.api import Holt
from statsmodels.tsa.api import ExponentialSmoothing
from sklearn.svm import SVR

import warnings
warnings.filterwarnings("ignore")

def Create_TimeIndex(df):
    df["Time"]=pd.to_datetime(df["Time"])
    x=(df["Time"].dt.year-2017)*12+df["Time"].dt.month
    return x

def create_dataset(dataset, look_back=1):
	dataX, dataY = [], []
	for i in range(len(dataset)-look_back-1):
		a = dataset[i:(i+look_back), 0]
		dataX.append(a)
		dataY.append(dataset[i + look_back, 0])
	return numpy.array(dataX), numpy.array(dataY)


def train_test(data):
    data["month"]=data["Time"].dt.month
    data["year"]=data["Time"].dt.year
    data["week_day"]=data["Time"].dt.weekday+1
    data["week"]=data["Time"].dt.week
    start_date=data.iloc[:1,6]
    
    data["TimeIndex"]=Create_TimeIndex(data)
    
    #Split
    #trainset=[]
    #testset=[]
    index=list(range(0,(len(data)-6)))
    testset=data.iloc[len(data)-6:,]
    newtestset = data.iloc[len(data)-4:,]
    newtestset = newtestset.reset_index()
    new_testset=testset.iloc[:,:8] #Drop Finance column
    trainset=data.iloc[index,]
    x_train=trainset.loc[:,["month","TimeIndex"]]
    y_train=trainset.loc[:,["Finance_Value"]]
    x_test=testset.loc[:,["month","TimeIndex"]]
    y_test=testset.loc[:,["Finance_Value"]]
    #scaler
    y_train = y_train.values
    y_train = y_train.reshape(-1, 1)
    y_test = y_test.values
    y_test = y_test.reshape(-1, 1)
    # normalize the dataset
    scaler = MinMaxScaler(feature_range=(0, 1))
    y_train = scaler.fit_transform(y_train)
    y_test = scaler.fit_transform(y_test)
    # reshape into X=t and Y=t+1
    look_back = 1
    trainX, trainY = create_dataset(y_train, look_back)
    testX, testY = create_dataset(y_test, look_back)
    # reshape input to be [samples, time steps, features]
    trainX = numpy.reshape(trainX, (trainX.shape[0], 1, trainX.shape[1]))
    testX = numpy.reshape(testX, (testX.shape[0], 1, testX.shape[1]))
    # create and fit the LSTM network
    model = Sequential()
    model.add(LSTM(4, input_shape=(1, look_back)))
    model.add(Dense(1))
    model.compile(loss="mean_squared_error", optimizer="adam")
    model.fit(trainX, trainY, epochs=50, batch_size=4, verbose=2)
    # make predictions
    trainPredict = model.predict(trainX)
    testPredict = model.predict(testX)
    # invert predictions
    trainPredict = scaler.inverse_transform(trainPredict)
    trainY = scaler.inverse_transform([trainY])
    testPredict = scaler.inverse_transform(testPredict)
    testY = scaler.inverse_transform([testY])
    # calculate root mean squared error
    trainScore = math.sqrt(mean_squared_error(trainY[0], trainPredict[:,0]))
    print("Train Score: %.2f RMSE" % (trainScore))
    testScore = math.sqrt(mean_squared_error(testY[0], testPredict[:,0]))
    # add the prediction value into the dataframe
    arr = np.array(testPredict)
    df = pd.DataFrame(data=arr.flatten())
    a = df.pop(0)
    newtestset["LSTM"] = a
    returnset=newtestset[["Account", "BusinessProcess", "Entity", "LineItemDetail", "Product", "Scenario", "Time", "TimeDataView", "Finance_Value", "LSTM"]]
    
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

#Consolidate all predictions in a dataframe
for i in range(0,len(combo)):
    temp=maindata[(maindata["Entity"]==combo.iloc[i,0]) & (maindata["Product"]==combo.iloc[i,1])]
    temp=temp.sort_values(by="Time")
    pred=train_test(temp) #contains all test dataset with model predicted values in each column
    predictions=predictions.append(pred)

'

,@input_data_1 = N'SELECT * FROM Data_py'
,@input_data_1_name = N'maindata'
,@output_data_1_name = N'predictions'
SELECT * FROM prediction_data_py;
END;
Go
EXEC Predict_n_Evaluate

use demosalespy;
select * from prediction_data_py;
select * from Data_py where Entity = 'LE6' and Product = 'A500';
Go
