#!/usr/bin/python3

import sys,csv, math

codesFile = open('../04c_euclidean/stampsBritannia.csv', 'r')
codes = csv.reader(codesFile, delimiter=';')
# header
codes.__next__()

sites = list()
listCodes = list()

for code in codes:
    idSite = code[0]
    nameSite = code[1]
    xSite = code[2]
    ySite = code[3]
    stamp = code[4]

    if idSite not in sites:
        sites.append(idSite)
        listCodes.append(list())
    index = sites.index(idSite)
    if stamp not in listCodes[index]:
        listCodes[index].append(stamp)

counts = open("countsBritannia.csv", "w")
counts.write("idSite;numCodes\n")

for i in range(len(sites)):
    counts.write(sites[i]+";"+str(len(listCodes[i]))+"\n")

