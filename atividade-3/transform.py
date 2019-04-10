import pandas as pd
import numpy as np
import matplotlib.pyplot as plt 

data = pd.read_csv('../dataset/train.csv')

qualitativeColumns = ['OverallQual', 'OverallCond', 'ExterQual', 'ExterCond', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2', 'HeatingQC', 'CentralAir', 'Electrical', 'KitchenQual', 'FireplaceQu', 'GarageFinish', 'GarageQual', 'GarageCond', 'PavedDrive', 'PoolQC', 'Fence']

grades = [
[
    'Ex', 'TA', 'Gd', 'Fa', 'Po', 'nan', 'Mn', 'Av', 'No', 'GLQ', 'ALQ', 'Unf', 'Rec', 'LwQ', 'BLQ', 'Y', 'N', 'sBrkr', 'FuseF', 'FuseA', 'FuseP', 'Mix', 'RFn', 'Fin', 'P'],
[
    10,      6,    8,    4,    2,   0,    4,    6,    2,     8,     5,     5,     5,     5,     4,  10,   0,      10,       6,       8,       4,     2,    10,     8,   5]
] 

classification = pd.DataFrame(grades).T

columnNames = data.columns.values

OverallQualitySum = pd.DataFrame(np.zeros(data.shape[0]))

for dataIndex, dataRow in data.iterrows():  # LINHAS DO DADO 
    for qualitativeColumn in qualitativeColumns:       # COLUNAS QUE QUEREMOS PERCORRER
        for qualitativeIndex, qualitativeRow in classification.iterrows():     # LINHAS        
            if (dataRow[qualitativeColumn] == qualitativeRow.iloc[0]):
                data[qualitativeColumn].iloc[dataIndex] = qualitativeRow.iloc[1]
                OverallQualitySum.iloc[dataIndex] += qualitativeRow.iloc[1] 

for qualitativeColumn in qualitativeColumns:
    del data[qualitativeColumn]

minimum = OverallQualitySum.min()
maximum = OverallQualitySum.max()

for index, elem in OverallQualitySum.iterrows():
    OverallQualitySum.iloc[index] = (elem - minimum)/(maximum - minimum)

data.insert(loc = 1, column="OverallQualitySum", value = OverallQualitySum )

data.to_csv('./modified-dataset/train.csv')
