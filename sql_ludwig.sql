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
#Packages required for Uber Ludwig
import pandas as pd
import numpy as np
import scipy
import spacy
import tensorflow
import matplotlib
import seaborn
import Cython
import h5py
import tqdm
import tabulate

import skimage

df["Time"]=pd.to_datetime(df["Time"])
#Create columns for year, month, week, and dummy variables for each month
df["year"]=df["Time"].dt.year
df["week_day"]=df["Time"].dt.weekday+1
df["week"]=df["Time"].dt.week
df["month"]=df["Time"].dt.month
dummies=pd.get_dummies(df["month"], prefix="month")
df=df.join(dummies)

pd.set_option("display.max_columns", None)

from ludwig import LudwigModel

#This is the YAML model definition that can be easily input for each variable. This will be the same for any data that
#is used. Simply input the column name and data type, and the model will handle the rest.
model_definition = {
    "input_features":[
        {"name":"Product", "type":"category"},
        {"name":"Entity", "type":"category"},
        {"name":"month","type":"category"},
        {"name":"year","type":"category"},
        
        
    ],
    "output_features": [
        {"name":"Finance_Value", "type":"numerical"},
    ]
}

print("creating model")
model = LudwigModel(model_definition)
print("training model")
train_stats = model.train(data_df=df)
model.close()

predictions = model.predict(data_df=df)
print(predictions.head())
'
,@input_data_1 = N'SELECT * FROM Data_py'
,@input_data_1_name = N'df'
--,@output_data_1_name = N'predictions'
--SELECT * FROM prediction_data_py;
END;
Go
EXEC Predict_n_Evaluate

use demosalespy;
select * from prediction_data_py;
select * from Data_py where Entity = 'LE6' and Product = 'A500';
Go