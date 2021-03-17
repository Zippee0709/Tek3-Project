#!/usr/bin/env python3
##
## EPITECH PROJECT, 2020
## 305Construction
## File description:
## 305Construction
##

import sys, os
import time
import math
import copy

def printUsage():
    print("USAGE\n./305Construction file\n\n \
    DESCRIPTION\n\tfile\tfile describing the tasks\n")

class Construction:
    def __init__(self):
        self.file = 0
        self.graph = Graph()
        self.totalDuration = 0
        self.start = []

    def runConstruction(self, file):
        try:
            self.file = open(file, "r")
            self.splitFile()
            self.parseFile()
            self.graph.granttAlgo(self.start)
        except OSError:
            raise OSError("Error: Read file failed !")
    
    def cleanList(self):
        while ("" in self.file):
            self.file.remove("")

    def splitFile(self):
        self.file = self.file.read()
        if (len(self.file) <= 0):
            raise ValueError("Error: Empty file !")
        self.file = self.file.split("\n")
        self.cleanList()
        for line in self.file:
            words = line.split(";")
            if (len(words) < 3):
                raise ValueError("Error: Invalid parsing on line: {} !".format(line))

    def printFile(self):
        for line in self.file:
            print(line)

    def parseFile(self):
        for line in self.file:
            words = line.split(";")
            if (len(words) <= 3):
                self.start.append(words[0])
            data = [words[1], words[2], words[3:]]
            self.graph.addNode(words[0], data)
    
    def printTotalDuration(self):
        if (self.totalDuration == 1):
            print("Total duration of construction: {} week".format(self.totalDuration))
        else:
            print("Total duration of construction: {} weeks".format(self.totalDuration))

class Graph:
    def __init__(self):
        self.vertices = 0
        self.graph = {}
        self.indexList = []
        self.finishedTaks = []
        self.totalDuration = 0

    def checkExistanceNode(self, id):
        if (id not in self.graph):
            self.graph[id] = []
            self.indexList.append(id)
            self.vertices += 1

    def addNode(self, src, data):
        self.checkExistanceNode(src)
        self.graph[src] = data
    
    def printGraph(self):
        for item in self.graph:
            print("Node : ({} : ".format(item), end='')
            for element in self.graph[item]:
                print("{}".format(element), end=",")
            print(")")

    def checkConstraints(self, src, listConstraints):
        for constraint in listConstraints:
            if (constraint == src):
                return True
        return False

    ## Formula is : s(i) = max(e(i)) for i in (1..n)
    def calculateTaskStartDate(self, task):
        constraints = self.graph[task][2]
        endDate = []
        for element in constraints:
            endDate.append(self.calculateTaskEndDate(element))
        if (len(endDate) == 0):
            return 0
        return max(endDate)


    ## Formula is : e(i) = s(i) + d(i)
    def calculateTaskEndDate(self, task):
        tmp = self.graph[task]
        endDate = self.calculateTaskStartDate(task) + int(tmp[1])
        return endDate

    ## Formula is : max(e(i)) for i in (1..n)
    def calculateTotalDuration(self):
        endDate = []
        for item in self.graph:
            endDate.append(self.calculateTaskEndDate(item))
        if (len(endDate) == 0) :
            print("Total duration of construction: 0 week")
            return
        maxDuration = max(endDate)
        self.totalDuration = maxDuration
        self.printTotalDuration(maxDuration)

    def printTotalDuration(self, duration):
        if (duration == 1):
            print("Total duration of construction: {} week".format(duration))
        else:
            print("Total duration of construction: {} weeks".format(duration))
        print("")

    ## Formula is : t'(i) = max(t'(j) - d(i)) for j in s(i)
    def calculateLastestStartDate(self):
        latestStartDate = dict()
        for item in self.graph:
            taskDuration = self.graph[item][1]
            startDate = []

            for element in self.graph:
                if element == item:
                    continue
                contraints = self.graph[element][2]
                if item in contraints:
                    startDate.append(self.calculateTaskStartDate(element))
            if (len(startDate) == 0):
                latestStartDate[item] = self.totalDuration - int(taskDuration)
            else :
                latestStartDate[item] = min(startDate) - int(taskDuration)
        latestStartDate = sorted(latestStartDate.items(), key=lambda x: x[1])
        latestStartDate = self.transformListTupleToDict(latestStartDate)
        return latestStartDate
        

    ## Formula is : t(i) = max(t(j) + d(j)) for j in p(i)
    def calculateEarliestStartDate(self):        
        earliestStartDate = dict()
        for item in self.graph:
            startDate = []
            constraints = self.graph[item][2]
            for element in constraints:
                startDate.append(self.calculateTaskEndDate(element))
            if (len(startDate) == 0):
                earliestStartDate[item] = 0
            else:
                earliestStartDate[item] = max(startDate)
        earliestStartDate = sorted(earliestStartDate.items(), key=lambda x: x[1])
        earliestStartDate = self.transformListTupleToDict(earliestStartDate)
        earliestStartDate = self.sortListDict(earliestStartDate.copy())
        return earliestStartDate

    def transformListTupleToDict(self, sources):
        newDict = dict()
        for element in sources:
            newDict[element[0]] = element[1]
        return newDict

    def sortListDict(self, sources):
        for element in sources:
            sources[element] = (sources[element], int(self.graph[element][1]), element)
        sources = sorted(sources.items(), key=lambda x: x[1])
        newDict = dict()
        for element in sources:
            tmpId = element[0]
            tmpStart = element[1][0]
            newDict[tmpId] = tmpStart
        return newDict

    def printSchedule(self, earliestStartDate, latestStartDate):
        for element in earliestStartDate:
            earliest = earliestStartDate[element]
            latest = latestStartDate[element]
            if (earliest == latest):
                print("{} must begin at t={}".format(element, earliest))
            else:
                print("{} must begin between t={} and t={}".format(element, earliest, latest))
        print("")
    
    def printBoard(self, earliestStartDate, latestStartDate):
        actualStart = 0

        for element in earliestStartDate:
            taskDuration = int(self.graph[element][1])
            earliest = earliestStartDate[element]
            latest = latestStartDate[element]
            diff = abs(earliest - latest)
            actualStart = earliest
            print("{}\t({})\t".format(element, diff), end="")
            secondPart = (" " * actualStart) + ("=" * taskDuration)
            print(secondPart)
            actualStart += taskDuration

    def granttAlgo(self, start):
        self.calculateTotalDuration()
        earliestStartDate = self.calculateEarliestStartDate()
        latestStartDate = self.calculateLastestStartDate()
        self.printSchedule(earliestStartDate, latestStartDate)
        self.printBoard(earliestStartDate, latestStartDate)

def main():
    if (len(sys.argv) == 2 and sys.argv[1] == "-h"):
        printUsage()
    elif (len(sys.argv) == 2):
        construction = Construction()
        construction.runConstruction(sys.argv[1])
    else:
        raise ValueError("Error: please look -h for using 305Construction")

if __name__ == "__main__":
    try:
        main()
    except (ValueError, ZeroDivisionError, OSError) as ve:
        print(ve)
        sys.exit(84)