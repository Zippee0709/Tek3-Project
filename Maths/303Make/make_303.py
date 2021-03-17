#!/usr/bin/env python3
##
## EPITECH PROJECT, 2020
## 301dannon
## File description:
## 301dannon
##

import sys, os
import time
import math
import copy

def printUsage():
    print("USAGE\n./301make makefile [file]\n\n \
    DESCRIPTION\n\tmakefile\tname of the makefile\n\
    \tfile\tname of recently modified file\n")

class Make:
    def __init__ (self):
        self.file = 0
        self.graph = 0

    def init2argv(self, file):
        try:
            self.file = open(file, "r")
            self.graph = Graph(float("inf"))
            self.parseFile()
            # self.graph.displayGraph()
            self.graph.graphToMatrix()
            self.graph.printMatrix()
            self.graph.depthFirstSearch()
        except OSError:
            raise OSError("Error: Read file failed.")

    def parseFile(self):
        data = self.file.read()
        lines = data.split("\n")
        for line in lines:
            if (line.find(":") >= 0):
                first = line[:line.find(":")]
                second = line[line.find(":") + len(":"):]
                second = second.split()
                for tmp in second:
                    self.graph.addNodes(tmp, first)

class Node:
    def __init__(self, id, weight):
        self._id = id
        self._weight = weight
    
    def getId(self):
        return self._id

    def getWeight(self):
        return self._weight

    def __str__(self):
        return "{0}: {1}".format(self._id, self._weight)
    
    def __repr__(self):
        return "Node(%s, %s)" % (self._id, self._weight)

class Graph:
    def __init__(self, maxLengthPath):
        self.INF = float('inf')
        self._maxLengthPath = maxLengthPath
        self._graph = {}
        self._matrix = []
        self._matrixShortPath = []
        self._vertice = 0
        self._indexList = []
        self._sortedGraph = []
        self._DpsResultList = []
        self._tmp = []

    def checkExistanceNode(self, id):
        if (id not in self._graph):
            self._graph[id] = []
            self._indexList.append(id)
            self._vertice += 1

    def addNodes(self, id, id2, weight = 1):
        self.checkExistanceNode(id)
        self.checkExistanceNode(id2)
        self._graph[id].append(Node(id2, weight))
        # self._graph[id2].append(Node(id, weight))

    def displayGraph(self):
        for item in self._graph:
            print("%s --> %s" % (item, self._graph[item]))
        print(self._indexList)

    def graphToMatrix (self):
        sortedIndexList = sorted(self._indexList)
        sortedGraph = sorted(self._graph.items())
        myList = list()

        for item in sortedGraph:
            myList.clear()
            for index in sortedIndexList:
                # print("index is = ", index)
                value = next((x for x in item[1] if x.getId() == index), False)
                myList.append(0 if value == False else 1)
            self._matrix.append(myList.copy())

    def depthFirstSearchUtils(self, i, items, visited):
        visited[i] = True
        i += 1
        if (len(items[1]) == 0):
            # print(items[0])
            self._tmp.append(items[0])
            self._DpsResultList.append(self._tmp.copy())
            self._tmp.clear()
            return

        for item in items[1]:
            # print(items[0], end=' -> ')
            self._tmp.append(items[0])
            id = item.getId()
            for tmp in self._sortedGraph:
                if (tmp[0] == id):
                    self.depthFirstSearchUtils(i, tmp, visited.copy())

    def depthFirstSearch(self):
        totalVertice = len(self._indexList)
        self._sortedGraph = sorted(self._graph.items())

        visited = [False] * totalVertice
        i = 0

        for item in self._sortedGraph:
            if (visited[i] == False):
                self.depthFirstSearchUtils(i, item, visited.copy())

        self._DpsResultList = sorted(self._DpsResultList)
        for item in self._DpsResultList:
            if (len(item) > 1):
                for element in item:
                    if (element == item[len(item) - 1]):
                        print(element)
                    else :
                        print(element, end=' -> ')

    def printMatrix(self):
        # print("matrix = ", self._matrix)
        for l in self._matrix:
            print("[", end='')
            print(" ".join(map(str, l)), end=']\n')
        print("")

def main():
    if (len(sys.argv) == 2 and sys.argv[1] == "-h"):
        printUsage()
    elif (len(sys.argv) == 2):
        newMake = Make()
        newMake.init2argv(sys.argv[1])
    else:
        raise ValueError("Error: please look -h for using 303make")

if __name__ == "__main__":
    try:
        main()
    except (ValueError, ZeroDivisionError, OSError) as ve:
        print(ve)
        sys.exit(84)