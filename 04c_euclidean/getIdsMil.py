#!/usr/bin/python3

import sys,csv, math

def findId(xStamp, yStamp, listXs, listYs):
    try:
        xIndex = listXs.index(xStamp)
        yIndex = listYs.index(yStamp)
        if xIndex!=yIndex:
            raise ValueError
    except ValueError:
        listXs.append(xStamp)
        listYs.append(yStamp)
    return listXs.index(xStamp)

stampsFile = open('../03_stampsWithProvince/05_britannia/military.csv', 'r')
stamps = csv.reader(stampsFile, delimiter=';')
# header
stamps.__next__()

sites = list()


locations = open("locationsBritannia.csv", "w")
locations.write("id;name;x;y;province;type\n")

stampsId = open("stampsBritannia.csv", "w")
stampsId.write("id;name;x;y;code\n")

listXs = list()
listYs = list()
listIds = list()

for stamp in stamps:
    xStamp = stamp[0]
    yStamp = stamp[1]
    # 3 is type - all dressel 20
    site = stamp[6]
    code = stamp[7]
    province = stamp[8]
    siteType = stamp[10]

    idSite= findId(xStamp, yStamp, listXs, listYs)
    stampsId.write(str(idSite)+";"+site+";"+str(xStamp)+";"+str(yStamp)+";"+code+"\n")

    # new site        
    if idSite not in sites:
        print("new site added:",idSite,site,siteType)
        locations.write(str(idSite)+";"+site+";"+str(xStamp)+";"+str(yStamp)+";"+province+";"+siteType+"\n")
        sites.append(idSite)

print("unique sites:",len(sites))
print(sites)
locations.close()
stampsId.close()

