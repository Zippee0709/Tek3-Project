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
    \t./308reedpipe r0 r5 r10 r15 r20 n\n\n\
    DESCRIPTION\n\
    \tr0\tradius (in cm) of pipe at the 0cm abscissa\n\
    \tr5\tradius (in cm) of pipe at the 5cm abscissa\n\
    \tr15\tradius (in cm) of pipe at the 10cm abscissa\n\
    \tr10\tradius (in cm) of pipe at the 15cm abscissa\n\
    \tn\tnumber of points needed to display the radius")

class ReedPipes:
    def __init__(self, args):
        self.numberOfPoints = int(args[6])
        self.maxLen = 5
        self.vectors = [0] * self.maxLen
        self.initialRadius = [None] * self.maxLen
        self.initialAbscissa = [0, 5, 10, 15, 20]
        self.abscissa = [None] * self.numberOfPoints
        self.radius = [None] * self.numberOfPoints
        self.pi = [0] * self.numberOfPoints
        self.stockData(args)

        self.matrix = [[20, 5, 0, 0, 0],
                       [5, 20, 5, 0, 0],
                       [0, 5, 20, 5, 0],
                       [0, 0, 5, 20, 5],
                       [0, 0, 0, 5, 20]]
        
        self.hi = [float(0)] * (self.maxLen)
        self.diffNewton = [float(0)] * (self.maxLen)
        self.coefBi = [float(0)] * (self.maxLen)

        # self.initialAbscissa = [0, 1, 3, 4, 0]
        # self.initialRadius = [1, 3, 5, 4, 0]

    def stockData(self, args):
        for i in range (0, 5):
            tmp = float(sys.argv[i + 1])
            if (tmp <= 0):
                raise ValueError("Error : Input must be positive value")
            self.initialRadius[i] = tmp
        for i in range(0, self.numberOfPoints):
            self.abscissa[i] = 20 / (self.numberOfPoints - 1) * i

    def calculateDifferenceOfX(self):
        for i in range(1, self.maxLen):
            self.hi[i - 1] = self.initialAbscissa[i] - self.initialAbscissa[i - 1]
        self.hi[self.maxLen - 1] = 5

    def calculateDifferenceOfNewton(self):
        for i in range(1, self.maxLen):
            self.diffNewton[i - 1] = (self.initialRadius[i] - self.initialRadius[i - 1]) / (self.initialAbscissa[i] - self.initialAbscissa[i - 1])

    def calculateCoefBi(self):
        for i in range(1, self.maxLen - 1):
            self.coefBi[i] = 6 * (self.diffNewton[i] - self.diffNewton[i - 1])
        # print(self.coefBi)
        # print(self.diffNewton)
        # print(self.hi)
 
    def calculateCoef(self):
        for i in range(1, self.maxLen - 1):
            aFirstDerivate = (self.initialRadius[i + 1] - self.initialRadius[i])
            bFirstDerivate = (self.initialAbscissa[i + 1] - self.initialAbscissa[i]) * (self.initialAbscissa[i + 1] - self.initialAbscissa[i - 1])
            firstDerivate = aFirstDerivate / bFirstDerivate
            aSecondDerivate = (self.initialRadius[i] - self.initialRadius[i - 1])
            bSecondDerivate = (self.initialAbscissa[i] - self.initialAbscissa[i - 1]) * (self.initialAbscissa[i + 1] - self.initialAbscissa[i - 1])
            secondDerivate = aSecondDerivate / bSecondDerivate
            # print("bFirstDerivate = {} --- bSecondDerivate = {}".format(bFirstDerivate, bSecondDerivate))
            self.vectors[i] = 6 * (firstDerivate - secondDerivate) / 2

        # # print("----------------------------------------")
        # print("self.vectors - res = ", res)
        # for i in range(1, self.maxLen - 1):
        #     a = (self.hi[i] / (self.hi[i] + self.hi[i + 1])) * self.vectors[i - 1]
        #     b = (2 * self.vectors[i] * (self.hi[i] + self.hi[i + 1]) / (self.hi[i] + self.hi[i + 1]))
        #     c = (self.hi[i + 1] / (self.hi[i] + self.hi[i + 1])) * self.vectors[i + 1]
        #     self.vectors[i] = a + b + c
        # print("self.vectors - res = ", res)

    def tridiagonalMatrix(self, a, b, c, d, e):
        nf = len(d) # number of equations
        ac, bc, cc, dc, ec = map(np.array, (a, b, c, d, e)) # copy arrays

        for it in range (1, nf):
            mc = ac[it-1]/bc[it-1]
            bc[it] = bc[it] - mc*cc[it-1] 
            dc[it] = dc[it] - mc*dc[it-1]
            ec[it] = ec[it] - mc*ec[it-1]

        xc = bc
        xc[-1] = dc[-1]/bc[-1]
        for il in range (nf - 2, -1, -1):
            xc[il] = (dc[il]-cc[il]*xc[il+1])/bc[il]
        return xc

    def calculateRadius(self):
        for i in range(self.numberOfPoints):
            index = int((self.abscissa[i] - 0.1) / 5) + 1
            # index = int(self.abscissa[i] * 20 / (self.numberOfPoints + 1))
            a = -self.vectors[index - 1] * (pow(self.abscissa[i] - self.initialAbscissa[index], 3) / (6 * self.hi[index - 1]))
            b =  self.vectors[index] * (pow(self.abscissa[i] - self.initialAbscissa[index - 1], 3) / (6 * self.hi[index - 1]))
            c = ((self.initialRadius[index - 1] * 6) / 6) - ((self.vectors[index - 1] * pow(self.hi[index], 2)) / 6)
            d = (self.abscissa[i] - self.initialAbscissa[index]) / self.hi[index - 1]
            e = ((self.initialRadius[index] * 6) / 6) - ((self.vectors[index] * pow(self.hi[index], 2)) / 6)
            f = (self.abscissa[i] - self.initialAbscissa[index - 1]) / self.hi[index - 1]
            self.radius[i] = a + b - c * d + e * f

    def printResult(self):
        for i in range(self.maxLen):
            self.vectors[i] = round(self.vectors[i], 1)
            if (self.vectors[i] == 0):
                self.vectors[i] = abs(self.vectors[i])
        print("vector result: ", self.vectors)
        for i in range(self.numberOfPoints):
            print("abscissa: {} cm\tradius: {} cm".format(round(self.abscissa[i], 1), round(self.radius[i], 1)))

    def printMatrix(self):
        for element in self.matrix:
            print(element)

def main():
    if (len(sys.argv) == 2 and sys.argv[1] == "-h"):
        printUsage()
    elif (len(sys.argv) == 7):
        newReedpipes = ReedPipes(sys.argv)
        newReedpipes.calculateDifferenceOfX()
        newReedpipes.calculateDifferenceOfNewton()
        newReedpipes.calculateCoefBi()
        newReedpipes.calculateCoef()
        newReedpipes.calculateRadius()
        newReedpipes.printResult()
    else:
        raise ValueError("Error: please look -h for using 308reedpipes")

if __name__ == "__main__":
    try:
        main()
        sys.exit(0)
    except ValueError as ve:
        print(ve)
        sys.exit(84)
