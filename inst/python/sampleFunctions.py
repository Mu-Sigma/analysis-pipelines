import pandas as pd
from sklearn import datasets
from sklearn import metrics
from sklearn.tree import DecisionTreeClassifier

def getColMeans(df):
    meanList = []
    for x in df.columns:
        meanList.append(df[x].mean())
    return meanList

def decisionTreeTrainAndTest(data, target, newData):
    model = DecisionTreeClassifier()
    model.fit(data, target)
    testPred = model.predict(newData)
    return testPred

