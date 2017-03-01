#!/usr/bin/python3

import csv, numpy

stampsFile = open("../04_locations/familyDr.csv", "r")
stamps = csv.reader(stampsFile, delimiter=';')

# header
stamps.__next__()

listSites = list()
listStamps = list()

for stamp in stamps:
    site = stamp[0]
    code = stamp[4]
    if site not in listSites:
        listSites.append(site)
        listStamps.append(list())

    siteIndex = listSites.index(site)
    stampsInSite = listStamps[siteIndex]
    if code not in stampsInSite:
        stampsInSite.append(code)


print("num sites:",len(listSites))

jaccard = numpy.zeros([len(listSites),len(listSites)])

for i in range(len(listSites)):
    for j in range(len(listSites)):
        if i == j:
            continue
         
        codesA = listStamps[i]
        codesB = listStamps[j]

        both = 0
        one = 0
        for code in codesA:
            if code in codesB:
                both += 1
            else:
                one += 1
        
        for code in codesB:
            if code not in codesA:
                one += 1

        distance = one/(one+both)                
        jaccard[i,j] = distance


numpy.savetxt("jaccardFamilyDr.txt", jaccard, fmt='%.3f')


