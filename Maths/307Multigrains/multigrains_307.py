#!/usr/bin/env python3

import sys, os
import time
import math

def printUsage():
    print("USAGE\n\
    \t./307multigrains n1 n2 n3 n4 po pw pc pb ps\n\n\
    DESCRIPTION\n\
    \tn1\tnumber of tons of fertilizer F1\n\
    \tn2\tnumber of tons of fertilizer F2\n\
    \tn3\tnumber of tons of fertilizer F3\n\
    \tn4\tnumber of tons of fertilizer F4\n\
    \tpo\tprice of one unit of oat\n\
    \tpw\tprice of one unit of wheat\n\
    \tpc\tprice of one unit of corn\n\
    \tpb\tprice of one unit of barley\n\
    \tps\tprice of one unit of soy")

class Resource:
    def __init__(self, F1, F2, F3, F4):
        self.value = {"F1" : F1, "F2" : F2, "F3" : F3, "F4" : F4}

    def __str__(self):
        return "{:.0f} F1, {:.0f} F2, {:.0f} F3, {:.0f} F4".format(self.value["F1"], self.value["F2"], self.value["F3"], self.value["F4"])


class Multigrains:
    def __init__(self):
        self.productionValue = 0
        self.resources = None
        self.grainsResource = {"oat": None, "wheat": None, "corn": None, "barley": None, "soy": None}
        self.grainsUnitPrice = {"oat": None, "wheat": None, "corn": None, "barley": None, "soy": None}
        self.grainsUnits = {"oat": None, "wheat": None, "corn": None, "barley": None, "soy": None}
        self.expr = None
        self.initData()

    def initData(self):
        self.resources = Resource(float(sys.argv[1]), float(sys.argv[2]), float(sys.argv[3]), float(sys.argv[4]))
        self.grainsResource["oat"] = Resource(1, 1, 2, 0)
        self.grainsResource["wheat"] = Resource(0, 2, 1, 0)
        self.grainsResource["corn"] = Resource(1, 0, 0, 3)
        self.grainsResource["barley"] = Resource(0, 1, 1, 1)
        self.grainsResource["soy"] = Resource(2, 0, 0, 2)
        self.grainsUnitPrice["oat"] = float(sys.argv[5])
        self.grainsUnitPrice["wheat"] = float(sys.argv[6])
        self.grainsUnitPrice["corn"] = float(sys.argv[7])
        self.grainsUnitPrice["barley"] = float(sys.argv[8])
        self.grainsUnitPrice["soy"] = float(sys.argv[9])

    def printGrainInfo(self):
        print(f'Oat: {0} units at ${self.grainsUnitPrice["oat"]}/unit')
        print(f'Wheat: {0} units at ${self.grainsUnitPrice["wheat"]}/unit')
        print(f'Corn: {0} units at ${self.grainsUnitPrice["corn"]}/unit')
        print(f'Barley: {0} units at ${self.grainsUnitPrice["barley"]}/unit')
        print(f'Soy: {0} units at ${self.grainsUnitPrice["soy"]}/unit')

    def printGrainResources(self):
        for elem in self.grainsResource:
            print("{}\t: {}".format(elem, self.grainsResource[elem]))

    def calculateLinearRelationships(self):
        self.expr = list()
        for i in range(4):
            tmpExpr = list()
            for elem in self.grainsResource:
                tmpExpr.append(list(self.grainsResource[elem].value.values())[i])
            for j in range(5):
                if (i == j):
                    tmpExpr.append(1)
                else:
                    tmpExpr.append(0)
            tmpExpr.append(list(self.resources.value.values())[i])
            self.expr.append(tmpExpr)
        tmpExpr = list()
        for elem in self.grainsUnitPrice.values():
            tmpExpr.append(-elem)
        for i in range(6):
            tmpExpr.append(0)
        self.expr.append(tmpExpr)

    def searchNewPivot(self):
        actualValuePrice = self.expr[4][:5]
        minValue = min(actualValuePrice)
        pivot_y = -1
        pivot_x = -1
        
        if (minValue >= 0):
            return pivot_y, pivot_x
        else:
            pivot_x = actualValuePrice.index(minValue)
        minValuesOfFertilizers = list()
        for i in range(4):
            if(self.expr[i][pivot_x] != 0):
                minValuesOfFertilizers.append(self.expr[i][-1] / self.expr[i][pivot_x])
            else:
                minValuesOfFertilizers.append(math.inf)
                pivot_y = i
        for i in range(4):
            if (minValuesOfFertilizers[i] < 0):
                 minValuesOfFertilizers[i] = math.inf
        pivot_y = minValuesOfFertilizers.index(min(minValuesOfFertilizers))
        return pivot_y, pivot_x        

    def applyPivotOperation(self, pivot_y, pivot_x):
        pivot_value = self.expr[pivot_y][pivot_x]
        maxX = len(self.expr[pivot_y])
        maxY = len(self.expr)
        for i in range(maxX):
            self.expr[pivot_y][i] /= pivot_value
        for i in range(maxY):
            if (i != pivot_y):
                coef = self.expr[i][pivot_x]
                for j in range(maxX):
                    self.expr[i][j] -= self.expr[pivot_y][j] * coef

    def simplexAlgorithm(self):
        result = [None] * 4
        pivot_y = None
        pivot_x = None

        while (pivot_x != -1 and pivot_y != -1):
            pivot_y, pivot_x = self.searchNewPivot()
            if (pivot_x != -1 and pivot_y != -1):
                self.applyPivotOperation(pivot_y, pivot_x)
                result[pivot_y] = pivot_x
        return result

    def printResult(self, res):
        ressourceProduction = [0] * 5
        totalProduction = 0
        for i in range(4):
            if (res[i] != None and self.expr[i][-1] != 0):
                ressourceProduction[res[i]] = self.expr[i][-1]
                totalProduction += self.expr[i][-1] * list(self.grainsUnitPrice.values())[res[i]]
        i = 0
        print("")
        for key, value in self.grainsUnitPrice.items():
            print("{}: ".format(key.capitalize()), end="")
            if (ressourceProduction[i] == 0):
                print("0 units at ", end="")
            else :
                print("{:.2f} units at ".format(ressourceProduction[i]), end="")
            print("${:.0f}/unit".format(value))
            i += 1
        print("\nTotal production value: ${:.2f}".format(self.expr[-1][-1]))

    def printExpr(self):
        for elem in self.expr:
            print(elem)

    # def compute(self):

def startMultigrains():
    try: 
        grains = Multigrains()
        print("Resources:", grains.resources)
        grains.calculateLinearRelationships()
        result = grains.simplexAlgorithm()
        grains.printResult(result)
    except ValueError:
        print("Error : In multigrains algorithm")

def main():
    if (len(sys.argv) == 2 and sys.argv[1] == "-h"):
            printUsage()
    elif (len(sys.argv) == 10):
        startMultigrains()
    else:
        raise ValueError("Error: please look -h for using 307multigrains")

if __name__ == "__main__":
    try:
        main()
        sys.exit(0)
    except ValueError as ve:
        print(ve)
        sys.exit(84)
