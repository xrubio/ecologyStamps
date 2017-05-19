#!/usr/bin/env python3

# Copyright (c) 2017 - Xavier Rubio-Campillo 
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version
#
# The source code is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#   
# You should have received a copy of the GNU General Public 
# License along with this library.  If not, see <http://www.gnu.org/licenses/>.


#### prepare data for analysis and store it in 2 files: a list of sites with provinces and an absence/presence matrix


import csv 

def extractSites(stampsFilename, amphType = None):
    stampsFile = open(stampsFilename)
    # ids is name of site and sites is province
    ids = []
    sites = []

    stamps = csv.reader(stampsFile, delimiter=';')
    # header
    stamps.__next__()

    for stamp in stamps:
        dbId = stamp[6]
        typology = stamp[5]

        if amphType and amphType != typology:
            continue

        province = stamp[8]
        if not province:
            continue

        if dbId not in ids:
            ids.append(dbId)
            sites.append(province)
    if amphType:
        print("parsed:",len(sites),"sites of type",amphType)
    else:
        print("parsed:",len(sites),"sites of all amphorae types")
    stampsFile.close()
    return sites,ids
        
def extractStamps(stampsFilename, amphType = None):
    # return a list of sites; for each site (identified for index) it gives a list of codes present there

    stampsFile = open(stampsFilename)
    #  
    ids = {}
    codesInSite = []
    uniqueCodes = []

    stamps = csv.reader(stampsFile, delimiter=';')
    # header
    stamps.__next__()

    for stamp in stamps:
        # if not in dictionary create it
        dbId = stamp[6]
        code = stamp[7]   
        typology = stamp[5]

        if amphType and amphType != typology:
            continue

        province = stamp[8]
        if not province:
            continue

        if dbId not in ids:
            ids[dbId] = len(codesInSite)
            codesInSite.append([])
        # collect index
        index = ids[dbId]
        # if code not in there add it (absence/presence, no freqs!)
        if code not in codesInSite[index]:
            codesInSite[index].append(code)
        if code not in uniqueCodes:
            uniqueCodes.append(code)
    stampsFile.close()

    return codesInSite,uniqueCodes

def writeSites(sites, names, output):
    sitesFile = open(output, "w")
    # header
    sitesFile.write("id;province;name\n")
    for i in range(len(sites)):
        sitesFile.write(str(i)+";"+sites[i]+";"+names[i]+"\n")
    sitesFile.close()    

def writePresenceMatrix(stampsInLocs, codes, output):
    presenceFile = open(output, "w")

    # header
    headerStr = "site"
    for code in codes:
        headerStr += ";"+code
    presenceFile.write(headerStr+'\n')

    for i in range(len(stampsInLocs)):
        siteStr = str(i)
        for code in codes:
            if code in stampsInLocs[i]:
                siteStr += ";1"
            else:            
                siteStr += ";0"
        presenceFile.write(siteStr+'\n')
        
def main():
    rawData = "../data/stamps.csv"

    # all 
    outputSites = "../data/sites.csv"
    outputPresence = "../data/presence.csv"

    sites, names = extractSites(rawData)
    stampsInLocs,codes = extractStamps(rawData)

    writeSites(sites, names, outputSites)
    writePresenceMatrix(stampsInLocs, codes, outputPresence)

if __name__ == "__main__":
    main()


