#!/usr/bin/python3

import csv, numpy

stampsFile = open("../04_locations/dr.csv", "r")
stamps = csv.reader(stampsFile, delimiter=';')

# header
stamps.__next__()


# create list of codes
listCodes = list()
for stamp in stamps:
    code = stamp[4]
    if code not in listCodes:
        listCodes.append(code)

print("total number of codes:",len(listCodes))

stampsFile.seek(0)
# header
stamps.__next__()


# loop and check
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

output = open("presenceDr.csv","w")

# header
headerStr = "site"
for code in listCodes:
    headerStr += ";"+code
output.write(headerStr+'\n')

for i in range(len(listSites)):
    siteId = listSites[i]
    siteStr = str(siteId)
    codes = listStamps[i]

    for code in listCodes:
        if code in codes:
            siteStr += ";1"
        else:            
            siteStr += ";0"
    output.write(siteStr+'\n')

