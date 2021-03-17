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
    print("USAGE\n./304pacman file c1 c2\n\n \
    DESCRIPTION\n\tfile\tfile describing a board, using the following character\n\
        \t\t'0' for empty square\n \
        \t\t'1' for a wall\n \
        \t\t'F' for for the ghost's position\n \
        \t\t'P' for Pacman's position\n \
    \tc1\tcharacter to display for a wall \n \
    \tc2\tcharacter to display for an empty case\n" )

class Pacman:
    def __init__ (self):
        self.file = 0
        self.graph = 0
        self.wall = 0
        self.emptyCase = 0
        self.maxY = 0
        self.maxX = 0
        self.newBoard = list()

    def init4argv(self, file, wallChar, emptyChar):
        try:
            self.wall = wallChar
            self.emptyCase = emptyChar
            self.file = open(file, "r")
            self.parseFile()
            self.graph = Graph(float("inf"))
            self.addNewBoardToGraph()
            self.graph.dijkstraAlgorithme(self.newBoard, self.wall, self.emptyCase)
            self.graph.printBoard()
            # self.graph.displayGraph()
        except OSError:
            raise OSError("Error: Read file failed.")

    def parseFile(self):
        data = self.file.read()
        if (len(data) <= 0):
            raise ValueError("Error: Empty file")
        lines = data.split("\n")
        tmp = list()

        nbP = 0
        nbF = 0

        for line in lines:
            for char in line:
                if (char == '0'):
                    tmp.append(self.emptyCase)
                elif (char == '1'):
                    tmp.append(self.wall)
                elif (char == 'F'):
                    tmp.append(char)
                    nbF += 1
                elif (char == 'P'):
                    tmp.append(char)
                    nbP += 1
                else:
                    raise ValueError("Error : Invalid Character found in given file !")
                if (nbF > 1 or nbP > 1):
                    raise ValueError("Error : Number of F and P is limited to 1 !")
            if (len(line) != 0):
                self.newBoard.append(tmp.copy())
                tmp.clear()

    def concatNodePosWithChar(self, y, x):
        # return (str(y) + "-" + str(x) + "-" + self.newBoard[y][x])
        return (str(y) + "-" + str(x))

    def getCharOnBoard(self, y, x):
        return self.newBoard[y][x]

    def addUpCase(self, x, y):
        if (y - 1 >= 0):
            tmpA = self.concatNodePosWithChar(y, x)
            tmpB = self.concatNodePosWithChar(y - 1, x)
            self.graph.addNodes(tmpA, tmpB, self.getCharOnBoard(y, x), self.getCharOnBoard(y - 1, x))

    def addDownCase(self, x, y):
        if (y + 1 <= self.maxY):
            tmpA = self.concatNodePosWithChar(y, x)
            tmpB = self.concatNodePosWithChar(y + 1, x)
            self.graph.addNodes(tmpA, tmpB, self.getCharOnBoard(y, x), self.getCharOnBoard(y + 1, x))

    def addLeftCase(self, x, y):
        if (x - 1 >= 0):
            tmpA = self.concatNodePosWithChar(y, x)
            tmpB = self.concatNodePosWithChar(y, x - 1)
            self.graph.addNodes(tmpA, tmpB, self.getCharOnBoard(y, x), self.getCharOnBoard(y, x - 1))

    def addRightCase(self, x, y):
        if (x + 1 <= self.maxX):
            tmpA = self.concatNodePosWithChar(y, x)
            tmpB = self.concatNodePosWithChar(y, x + 1)
            self.graph.addNodes(tmpA, tmpB, self.getCharOnBoard(y, x), self.getCharOnBoard(y, x + 1))

    def addNewBoardToGraph(self):        
        self.maxY = len(self.newBoard) - 1
        self.maxX = len(self.newBoard[0]) - 1

        for y in range(self.maxY):
            for x in range(self.maxX):
                self.addUpCase(x, y)
                self.addRightCase(x, y)
                self.addDownCase(x, y)
                self.addLeftCase(x, y)

    def printNewBoard(self):
        if (len(self.newBoard) == 0):
            raise ValueError("Error: can't print empty board")

        for line in self.newBoard:
            for char in line:
                print(char, end='')
            print("")
        

class Node:
    def __init__(self, id, weight):
        self._id = id
        self._weight = weight
    
    def getId(self):
        return self._id

    def setId(self, newId):
        self.id = newId

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
        self._board = []
        self._tmp = []
        self._wall = 0
        self._emptyCase = 0
        self._visited = []
        self._nextToVisit = []
        self._indexListTuple = []

    def checkExistanceNode(self, id, char):
        if (id not in self._graph):
            self._graph[id] = []
            self._indexList.append(id)
            self._indexListTuple.append((id, char))
            self._vertice += 1

    def addNodes(self, id, id2, charId, charId2, weight = 1):
        self.checkExistanceNode(id, charId)
        self.checkExistanceNode(id2, charId2)
        self._graph[id].append(Node(id2, weight))

    def displayGraph(self):
        for item in self._graph:
            # print("item = ", item)
            print("%s --> %s" % (item, self._graph[item]))
        print(self._indexList)

    def searchCharInGraph(self, char):
        for tmpTuple in self._indexListTuple:
            if (tmpTuple[1] == char):
                return tmpTuple
    
    def searchTupleIntGraph(self, itemId):
        for tmpTuple in self._indexListTuple:
            if (tmpTuple[0] == itemId):
                return tmpTuple

    def checkIfVisited(self, itemId):
        if (len(self._visited) == 0):
            return False

        for tmpId in self._visited:
            if (tmpId == itemId):
                return True
        return False

    def changeIdInTupleList(self, itemId, count):
        for tmpTuple in self._indexListTuple:
            if (tmpTuple[0] == itemId):
                self._indexListTuple.remove(tmpTuple)
                self._indexListTuple.append((itemId, str(count)))
                return tmpTuple
    
    def dijkstraAlgoUtils(self, pacmanChar, count):
        if (len(self._nextToVisit) == 0):
            return
        tmplist = list()

        for node in self._nextToVisit:
            tmp = self.searchTupleIntGraph(node)
            if (self.checkIfVisited(node) == True):
                continue
            if (tmp[1] == pacmanChar):
                return
            if (tmp[1] == self._wall):
                continue
            if (tmp[1] == self._emptyCase):
                self.changeIdInTupleList(node, count if count < 10 else count % 10)
                tmpPos = tmp[0].split("-")
                self._board[int(tmpPos[0])][int(tmpPos[1])] = str(count if count < 10 else count % 10)
                self._visited.append(node)
            for item in self._graph[node]:
                tmplist.append(item.getId())
        self._nextToVisit = tmplist
        count += 1
        self.dijkstraAlgoUtils(pacmanChar, count)

    def dijkstraAlgorithme(self, board, wall, emptyCase):
        pacmanChar = self.searchCharInGraph('P')
        ghostChar = self.searchCharInGraph('F')
        self._board = board
        self._wall = wall
        self._emptyCase = emptyCase
        count = 0
        self._nextToVisit.append(ghostChar[0])
        self.dijkstraAlgoUtils(pacmanChar[1], count)

    def printBoard(self):
        if (len(self._board) == 0):
            raise ValueError("Error: can't print empty board")
        for line in self._board:
            for char in line:
                print(char, end='')
            print("")

def main():
    if (len(sys.argv) == 2 and sys.argv[1] == "-h"):
        printUsage()
    elif (len(sys.argv) == 4):
        newPacman = Pacman()
        newPacman.init4argv(sys.argv[1], sys.argv[2], sys.argv[3])
    else:
        raise ValueError("Error: please look -h for using 304pacman")

if __name__ == "__main__":
    try:
        main()
    except (ValueError, ZeroDivisionError, OSError) as ve:
        print(ve)
        sys.exit(84)