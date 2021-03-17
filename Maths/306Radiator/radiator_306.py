#!/usr/bin/env python3
##
## EPITECH PROJECT, 2020
## 306radiator
## File description:
## 306radiator
##

import sys, os
import math
import numpy as np

def printUsage():
    print("USAGE\n./306radiator n ir jr (i j)\n\n \
    DESCRIPTION\n\
    \tn\tsize of the room\n \
    \t(ir, jr)\tcoordinates of the radiator\n \
    \t(i, j)\tcoordinates of a point in the room")

class Radiator:
    def __init__(self):
        self.h = 0.5
        self.f = -300

        self.roomSize = None
        self.N = None
        self.ir = None
        self.jr = None
        self.i = None
        self.j = None
        self.coefMatrix = None
        self.vectorMatrix = None

    def init(self, n , ir, jr, i = None, j = None):
        try : 
            self.roomSize = int(n)
            self.N = self.roomSize ** 2
            self.ir = int(ir)
            self.jr = int(jr)
            if (i != None and j != None):
                self.i = int(i)
                self.j = int(j)
        except OSError:
            raise OSError("Error : The given av must be int number")

    def run(self, av):
        if (len(av) == 4):
            self.init(av[1], av[2], av[3])
        elif (len(av) == 6):
            self.init(av[1], av[2], av[3], av[4], av[5])
        self.errorGestion()
        self.buildMatrix()
        self.buildVector()
        newGaussian = Gaussian(self.coefMatrix, self.vectorMatrix, self.i, self.j)
        if (self.i == None and self.j == None):
            newGaussian.displayMatrix()
            newGaussian.gaussian()
            newGaussian.displayVector()
        else:
            newGaussian.gaussian()
            newGaussian.displayCoordinateResult()

    def errorGestion(self):
        if (self.roomSize <= 0):
            raise ValueError("Error : The room size must be greater than 0")
        if (self.ir <= 0 or self.jr <= 0 or self.ir >= self.roomSize - 1 or self.jr >= self.roomSize - 1):
            raise ValueError("Error : The coordinate of radiator is invalid")
        if (self.i != None and self.j != None):
            if (self.i <= 0 or self.j <= 0 or self.i >= self.roomSize - 1 or self.j >= self.roomSize - 1):
                raise ValueError("Error : The coordinate given to search is invalid")

    def buildMatrix(self):
        self.coefMatrix = [0] * self.N
        for i in range(0, self.N):
            self.coefMatrix[i] = [0] * self.N
        counter = 0
        for i in range(0, self.roomSize):
            for j in range(0, self.roomSize):
                if ((i == 0 or i == self.roomSize - 1) or (j == 0 or j == self.roomSize - 1)):
                    self.calculateBoundaryCondition(counter)
                elif (i >= 1 or i <= self.roomSize - 2) or (j >= 1 or j <= self.roomSize - 2):
                    self.calculateSecondDerivate(counter)
                counter += 1

    def calculateSecondDerivate(self, x):
        for y in range(self.N):
            if (y == x):
                # resX = (self.coefMatrix[x - 1][y] - (4 * self.coefMatrix[x][y]) + self.coefMatrix[x + 1][y]) / (self.h ** 2)
                # resY = (self.coefMatrix[x][y - 1] - (4 * self.coefMatrix[x][y]) + self.coefMatrix[x][y + 1]) / (self.h ** 2)
                # self.coefMatrix[x][y] = resX + resY
                self.coefMatrix[x][y] = (- 4 / (self.h ** 2))
            elif (y == x - 4 or y == x + 4 or y == x - 1 or y == x + 1):
                # resX = (self.coefMatrix[x - 1][y] - (4 * self.coefMatrix[x][y]) + self.coefMatrix[x + 1][y]) / (self.h ** 2)
                # resY = (self.coefMatrix[x][y - 1] - (4 * self.coefMatrix[x][y]) + self.coefMatrix[x][y + 1]) / (self.h ** 2)
                # self.coefMatrix[x][y] = resX + resY
                self.coefMatrix[x][y] = (1 / (self.h ** 2))
            else:
                self.coefMatrix[x][y] = 0

    def calculateBoundaryCondition(self, x):
        for y in range(self.N):
            if (y == x):
                self.coefMatrix[x][y] = 1
            else:
                self.coefMatrix[x][y] = 0

    def buildVector(self):
        self.vectorMatrix = [0] * self.N
        counter = 0
        for i in range(self.roomSize):
            for j in range(self.roomSize):
                if (i == self.ir and j == self.jr):
                    self.vectorMatrix[counter] = self.f
                else :
                    self.vectorMatrix[counter] = 0
                counter += 1

class Gaussian:
    def __init__(self, matrix, vector, i = None, j = None):
        self.matrix = matrix
        self.vector = vector
        self.N = len(matrix)
        self.vectorRes = None
        self.i = i
        self.j = j

    def gaussian(self):
        for i in range(0, self.N):
            maxValuePos = self.findMaxValuePos(i)
            self.gaussianSwap(self.matrix[i], i, maxValuePos)
            self.gaussianSwap(self.vector, i, maxValuePos)
            self.gaussianNormalization(i)

        self.vectorRes = self.triangularMatrix()

    def gaussianNormalization(self, i):
        for k in range(i + 1, self.N):
            x = float(self.matrix[k][i] / self.matrix[i][i])
            self.gaussianCancelMatrix(k, i, -x)
            self.gaussianCancelVector(k, i, -x)

    def gaussianCancelMatrix(self, i, j, x):
        for k in range(0, self.N):
            self.matrix[i][k] += x * self.matrix[j][k]

    def gaussianCancelVector(self, i, j, x):
        self.vector[i] += x * self.vector[j]

    def triangularMatrix(self):
        x = [0] * self.N
        for i in range(self.N - 1, -1, -1):
            s = 0
            for j in range(i, self.N):
                s += self.matrix[i][j] * x[j]
            x[i] = (self.vector[i] - s) / self.matrix[i][i]
        return x

    def findMaxValuePos(self, i):
        absList = [abs(ele) for ele in self.matrix[i]]
        maxValue = max(absList)
        pos = absList.index(maxValue)
        return pos
    
    def gaussianSwap(self, array, i, j):
        tmp = array[i]
        array[i] = array[j]
        array[j] = tmp

    def displayMatrix(self):
        for x in range(0, self.N):
            for y in range(0, self.N):
                if (y == self.N - 1):
                    print(self.matrix[x][y])
                else:
                    print(self.matrix[x][y], end='\t')
    
    def displayVector(self):
        print("")
        for element in self.vectorRes:
            print("{0:.1f}".format(self.myRound(element)))

    def displayCoordinateResult(self):
        roomSize = int(math.sqrt(self.N))
        counter = 0
        x = 0
        for i in range(0, roomSize):
            for j in range(0, roomSize):
                if (i == self.i and j == self.j):
                    x = counter
                counter += 1
        print("{0:.1f}".format(self.myRound(self.vectorRes[x])))

    def myRound(self, n):
        tmp = round(n, 2) * 100
        if (tmp % 10 == 5):
            tmp += 1
        tmp /= 100
        return tmp
            

def main():
    if (len(sys.argv) == 2 and sys.argv[1] == "-h"):
        printUsage()
    elif (len(sys.argv) == 4 or len(sys.argv) == 6):
        newRadiator = Radiator()
        newRadiator.run(sys.argv)
    else:
        raise ValueError("Error: please look -h for using 306radiator")

if __name__ == "__main__":
    try:
        main()
    except (ValueError, ZeroDivisionError, OSError) as ve:
        print(ve)
        sys.exit(84)