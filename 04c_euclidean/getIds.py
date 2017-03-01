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


# all stamps    
#stampsFile = open('../03_stampsWithProvince/output/all.csv', 'r')
#locations = open("locations.csv", "w")
#stampsId = open("all.csv", "w")

# dressel 20
# stampsFile = open('../03_stampsWithProvince/output/dr.csv', 'r')
# locations = open("locationsDr.csv", "w")
# stampsId = open("dr.csv", "w")

# all stamps by family
stampsFile = open('../03_stampsWithProvince/output/family.csv', 'r')
locations = open("locationsFamily.csv", "w")
stampsId = open("family.csv", "w")

# all dressel 20 by family    
#stampsFile = open('../03_stampsWithProvince/output/familyDr.csv', 'r')
#locations = open("locFamilyDr.csv", "w")
#stampsId = open("familyDr.csv", "w")


locations.write("id;name;x;y;province\n")
stampsId.write("id;name;x;y;code\n")

stamps = csv.reader(stampsFile, delimiter=',')
# header
stamps.__next__()

sites = list()


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

    if not province:
        continue

    idSite= findId(xStamp, yStamp, listXs, listYs)
    stampsId.write(str(idSite)+";"+site+";"+str(xStamp)+";"+str(yStamp)+";"+code+"\n")

    # new site        
    if idSite not in sites:
        print("new site added:",idSite,site)
        locations.write(str(idSite)+";"+site+";"+str(xStamp)+";"+str(yStamp)+";"+province+"\n")
        sites.append(idSite)

print("unique sites:",len(sites))
print(sites)
locations.close()
stampsId.close()

