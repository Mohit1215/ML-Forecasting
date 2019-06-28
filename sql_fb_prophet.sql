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

import pandas as pd
import numpy as np

from fbprophet import Prophet

def train_test(df):

	index=list(range(0,(len(df)-6)))
	testset=df.iloc[len(df)-6:,]
	new_testset=testset.iloc[:,:8] #Drop Finance column
	trainset=df.iloc[index,]
	trainset = trainset[["Time","Finance_Value"]]
	trainset["Time"] = pd.to_datetime(trainset["Time"])
	trainset = trainset.rename(columns = {"Time":"ds","Finance_Value":"y"})

	new_testset = new_testset[["Time"]]
	new_testset["Time"] = pd.to_datetime(new_testset["Time"])
	new_testset = new_testset[["Time"]]
	m = Prophet(yearly_seasonality=True, weekly_seasonality=True)
	m.add_country_holidays(country_name = "US")
	m.fit(trainset)

	future = m.make_future_dataframe(periods=12, freq = "m") 
	print("This is the new data frame for future predictions")
	print(future.tail())

	forecast = m.predict()
	#forecast["Entity"] = trainset["Entity"]
	#forecast["Product"] = trainset["Product"]
	print(forecast.tail())
	print(forecast[["ds","yhat","yhat_lower","yhat_upper"]].tail())
"""

new1 = newdata[(newdata["Store"] == 1) & (newdata["Dept"] == 1)]

new1.describe()

new2 = new1[["Date","Weekly_Sales"]]

new2["Date"] = pd.to_datetime(new2["Date"])

new2 = new2.rename(columns={"Date":"ds", "Weekly_Sales":"y"})

index=list(range(0,(int(len(new2))-10)))
testset=new2.iloc[(len(new2)-10):,]
trainset=new2.iloc[index,]


m2 = Prophet(yearly_seasonality=True, weekly_seasonality=True)
m2.add_country_holidays(country_name = "US")
m2.fit(trainset)

future = m2.make_future_dataframe(periods=len(testset), freq = "w") 
future.tail()
forecast = m2.predict(future)

forecast[["ds","yhat","yhat_lower","yhat_upper"]].tail()

"""

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
#for i in range(0,len(combo)):
temp=maindata[(maindata["Entity"]=="LE6") & (maindata["Product"]=="A500")]# ==combo.iloc[i,0]) & (maindata["Product"]==combo.iloc[i,1])] 
temp=temp.sort_values(by="Time", ascending=True)

pred=train_test(temp) #contains all test dataset with model predicted values in each column
predictions=predictions.append(pred) #Append predictions on test data for every combination
print(predictions.tail())

###
##########################################################
'

,@input_data_1 = N'SELECT * FROM Data_py'
,@input_data_1_name = N'maindata'
--,@output_data_1_name = N'predictions'
--SELECT * FROM prediction_data_py;
END
Go
EXEC Predict_n_Evaluate

use demosalespy;
select * from prediction_data_py;
select * from Data_py where Entity = 'LE6' and Product = 'A500';
Go








