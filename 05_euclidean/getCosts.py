#!/usr/bin/python3

import csv
import numpy
import math

def getCost(i, j, listXs, listYs):
    if i==j:
        return 0

    dist = math.sqrt(math.pow(listXs[i]-listXs[j],2)+math.pow(listYs[i]-listYs[j],2))
    return dist 

locationsFile = open("../04_locations/locationsFamily.csv", "r")
locationsCSV = csv.reader(locationsFile,delimiter=";")
locationsCSV.__next__()

locations = list()
listXs = list()
listYs = list()

for location in locationsCSV:
    # store id
    locations.append(location[0])

    # km
    listXs.append(float(location[2])/1000)
    listYs.append(float(location[3])/1000)


print("number of locations:",len(locations))    
costs = numpy.zeros([len(locations),len(locations)])

i = 0
j = 0

for i in range(len(locations)):
    for j in range(len(locations)):
        origin = locations[i]
        target = locations[j]

        print("computing route from",i,"id:",origin,"to:",j,"id:",target)
        
        cost = getCost(i, j, listXs, listYs)
        costs[i,j] = cost

print(costs)    
numpy.savetxt("geoCostFamily.txt", costs, fmt='%.3f')

