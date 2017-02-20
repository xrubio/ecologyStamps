#!/usr/bin/python
# -*- coding: utf-8 -*-

import argparse
import urllib
import requests
import xml.dom.minidom

def executeQuery(sparql, verbose):

    user_agent = {'User-Agent': 'Mozilla/5.0','Accept':'text/html,application/xml'}

    prefix='PREFIX : <http://www.semanticweb.org/ontologies/2015/1/EPNet-ONTOP_Ontology#> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX dcterms: <http://purl.org/dc/terms/>'
    query=urllib.quote(prefix+sparql)
    #r will store the XML answer of the query
    r=requests.get("http://136.243.8.213:8080/openrdf-sesame/repositories/epnet_pleiades_edh_ads?query="+query,headers=user_agent)

    if verbose:
        print(r.content)

    allAmphoras=""
    try:
        allAmphoras=xml.dom.minidom.parseString(r.content)
    except:
        print('Error - answer is not a valid response: '+allAmphoras)
        return None
    return allAmphoras

def createOutput(queryResult, outputFileName, delimiter):

    outputFile = open(outputFileName, 'w')
    # TODO write header

    numResults = len(queryResult.getElementsByTagName('result'))
    print('total results:',numResults)

    i = 0
    for result in queryResult.getElementsByTagName('result'):
        newEntry = str(i)
        for literal in result.getElementsByTagName('literal'):
            if len(literal.childNodes) == 0:
                print("wrong entry due to a literal having 0 nodes")
                newEntry = None
                break
            else:
                newEntry += delimiter
                literalStr = literal.childNodes[0].data.encode('utf-8')
                newEntry += literalStr
        if newEntry:            
            outputFile.write(newEntry+'\n')
        i += 1

def readQuery(queryFileName):
    with open (queryFileName, "r") as queryFile:
        query = queryFile.read().replace('\n', '')
        return query
    return ''


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument("-o", "--output", help="CSV file to store output", default="out.csv")
    parser.add_argument("-d", "--delimiter", help="delimiter between fields", default=";")
    parser.add_argument("-qf", "--queryFile", help="file with query")
    parser.add_argument("-q", "--query", help="direct query")
    parser.add_argument("-r", "--result", help='parse result from already executed query', default=None)
    parser.add_argument("-v", "--verbose", help="verbose actions", default=False)
    
    args = parser.parse_args()

    if not args.query and not args.queryFile and not args.result:
        print('Error - no query asked or result added')
        return -1

    queryResult = None        
    if args.result:
        resultFile = open(args.result, 'r')
        resultStr = resultFile.read()
        queryResult = xml.dom.minidom.parseString(resultStr)
        resultFile.close()
    else:        
        query = ''
        if args.query:
            query = args.query
        elif args.queryFile:
            query = readQuery(args.queryFile)
        else:
            print('Error - conflicting options query and queryFile. Use only one of them')
            return -1
        queryResult = executeQuery(query, args.verbose)
    createOutput(queryResult, args.output, args.delimiter)

if __name__ == "__main__":
    main()

