import pandas as pd
import numpy as np
import math
from operator import itemgetter
import matplotlib.pyplot as plt

def distance(d1, d2):
    d = 0
    for x in range(len(d1)):
        d += np.square(d1[x] - d2[x])
    return np.sqrt(d)

def knn(train, test, k):
    distances = [[],[]]
    sort = []
    
    # MEASURE DISTANCE BETWEEN SAMPLE AND ALL TEST DATA
    for line in range(train.shape[0] - 1):
        d = distance(test, train.iloc[line,0:4].values)
        distances[0].append(d)
        distances[1].append(train.iloc[line,-1])

    # SORT DISTANCES
    sorted_d = pd.DataFrame(distances).T.sort_values(by=0)

    # GET K NEIGHBORS
    neighbors = [[],[]]
    for x in range(k):
        neighbors[0].append(sorted_d[0].iloc[x])
        neighbors[1].append(sorted_d[1].iloc[x])
    neighbors = pd.DataFrame(neighbors)

    return neighbors.iloc[1,:].value_counts().index[0]


train = pd.read_csv('./iris.csv', header = None)

for div in range(10,train.shape[0],10):
    chart = [[],[]]
    # RANDOM WITH SAME TRAIN SET
    random = train.sample(train.shape[0])
    randomTrain = random.iloc[0:div,:]
    randomTest = random.iloc[div:,:]
    for k in range(1,51):
        chart[0].append(k)
        err = 0
        for x in range(random.shape[0]/2):
            test = randomTest.iloc[x,0:4].values
            result = knn(randomTrain, test, k)
            
            if result == randomTest.iloc[x,4]:
                # print "[CORRECT] Predicted: " + str(result)
                pass
            else:
                # print "[INCORRECT] Predicted : " + str(result) + " expected " + str(randomTest.iloc[x,4])
                err += 1

        chart[1].append(err)

    plt.figure(1)
    plt.plot(chart[0], chart[1])
    plt.savefig("./randomSameTrainSet" + str(div) + ".png", dpi = 500)
    plt.close()


for div in range(10,train.shape[0],10):
    chart = [[],[]]
    # RANDOM WITH SAME TRAIN SET
    for k in range(1,51):
        random = train.sample(train.shape[0])
        randomTrain = random.iloc[0:div,:]
        randomTest = random.iloc[div:,:]
        chart[0].append(k)
        err = 0
        for x in range(random.shape[0]/2):
            test = randomTest.iloc[x,0:4].values
            result = knn(randomTrain, test, k)
            
            if result == randomTest.iloc[x,4]:
                # print "[CORRECT] Predicted: " + str(result)
                pass
            else:
                # print "[INCORRECT] Predicted : " + str(result) + " expected " + str(randomTest.iloc[x,4])
                err += 1

        chart[1].append(err)

    plt.figure(1)
    plt.plot(chart[0], chart[1])
    plt.savefig("./randomDiffTrainSet" + str(div) + ".png", dpi = 500)
    plt.close()