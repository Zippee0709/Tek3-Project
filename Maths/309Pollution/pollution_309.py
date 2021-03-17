#!/usr/bin/env python3
##
## EPITECH PROJECT, 2021
## B-MAT-500-PAR-5-1-308reedpipes-zhiwen.wang
## File description:
## multigrains_30711
##

import sys, os
import time
import math
import numpy as np

def printUsage():
    print("USAGE\n\
    \t./309pollution n file x y\n\n\
    DESCRIPTION\n\
    \tn\tnumber of points on the grid axis\n\
    \tfile\tcsv file containing the data points x;y;p\n\
    \tx\tabscissa of the point whose pollution level we want to know\n\
    \ty\tordinate of the point whose pollution level we want to know\n")

class Pollution:
    def _init(self):
        self.n = None
        self.dataPoints = None
        self.xToPrint = None
        self.yToPrint = None
        self.matrix = None
    
    def initValue(self):
        try :
            self.n = int(sys.argv[1])
            if (self.n <= 0):
                raise ValueError("Error : The number of points must be positive")
            self.dataPoints = self.convertDataToFloat(self.readFile(sys.argv[2]))
            if (self.dataPoints == None):
                raise ValueError("Error : error when reading file")
            self.xToPrint = float(sys.argv[3])
            self.yToPrint = float(sys.argv[4])
            if (self.xToPrint < 0 or self.yToPrint > self.n - 1):
                raise ValueError("Error : given x, y need must be between 0 to n - 1")
        except ValueError as ve:
            raise ve

    def readFile(self, fileName):
        try : 
            newfile = open(fileName, 'r')
            contents = newfile.read().splitlines()
            newfile.close()
            while "" in contents:
                contents.remove("")
            return contents
        except:
            raise ValueError("Error : error when reading file")

    def convertDataToFloat(self, contents):
        try :
            dataPoints = [None] * len(contents)
            i = 0
            for element in contents:
                line = element.split(";")
                dataPoints[i] = {'x' : int(line[0]), 'y' : int(line[1]), 'p' : float(line[2])}
                i += 1
            return dataPoints
        except:
            raise ValueError("Error : cannot convert file value into float")

    def printMatrix(self, tab):
        for element in tab:
            print(element)

    def initMatrix(self):
        self.matrix = [[None for x in range(self.n)] for y in range(self.n)]
        for x in range(0, self.n):
            for y in range(0, self.n):
                givenTmp = self.checkInfoDataPoints(x, y)
                if (givenTmp != None):
                    self.matrix[x][y] = givenTmp
                else:
                    self.matrix[x][y] = 0

    def checkInfoDataPoints(self, x, y):
        for element in self.dataPoints:
            if (element["x"] == x and element["y"] == y):
                return element["p"]
        return None

    def run(self, av):
        self.initValue()
        self.initMatrix()
        self.calculateBezierSurface(self.xToPrint, self.yToPrint)

    def calculateBezierSurface(self, u, v):
        # print(self.bezierCurve([0, 50, 80], 20))
        y = int(self.yToPrint * 10)
        x = int(self.xToPrint * 10)
        xResult = []
        for i in range(self.n):
            tmp = self.bezierCurve(self.matrix[i], 20)
            tmp.append(self.matrix[i][self.n - 1])
            xResult.append(tmp)
        res = []
        last = None
        for j in range(self.n):
            last = xResult[j][y]
            res.append(xResult[j][y])
        finalRes = self.bezierCurve(res, 20)
        finalRes.append(last)
        print("{:.2f}".format(finalRes[x]))

    def bezierCurve(self, controlPoints, nbOfCurve):
        # last_point = nbOfCurve - 1
        res = []
        for i in range(nbOfCurve):
            res.append(self.bezierPoint(controlPoints, i / nbOfCurve))
        return res

    def bezierPoint(self, controlPoints, t):
        if len(controlPoints) == 1:
            return round(controlPoints[0], 2)
        control_linestring = zip(controlPoints[:-1], controlPoints[1:])
        return self.bezierPoint([(1 - t) * p1 + t * p2 for p1, p2 in control_linestring], t)
    
def main():
    if (len(sys.argv) == 2 and sys.argv[1] == "-h"):
        printUsage()
    elif (len(sys.argv) == 5):
        newPollution = Pollution()
        newPollution.run(sys.argv)
    else:
        raise ValueError("Error: please look -h for using 309pollution")


if __name__ == "__main__":
    try:
        main()
        sys.exit(0)
    except ValueError as ve:
        print(ve)
        sys.exit(84)
