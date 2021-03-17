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
    print("USAGE\n./301dannon file\n\nDESCRIPTION\n\tfile\t file that contains the numbers to be sorted, seperated by spaces\n")

class Dannon:
    def __init__(self, file):
        try: 
            self.file = open(file, "r")
            self.listNumber = list()
            self.lenListNumber = 0
            self.quickSortIt = 0
            self.parseNumber()
        except OSError:
            raise OSError("Error: Read file failed.")
        except ValueError:
            raise ValueError("Error: the file can only contain numbers.")

    def parseNumber(self):
        data = self.file.read()
        self.listNumber = data.split()
        self.file.close()
        self.lenListNumber = len(self.listNumber)
        for i in range(0, self.lenListNumber):
            self.listNumber[i] = float(self.listNumber[i])
        print("{} elements".format(self.lenListNumber))

    def selectSort(self):
        myMin = 0
        tmp = 0
        tmpNum = 0
        nbComparisons = 0
        listNbr = self.listNumber.copy()
        for i in range(0, self.lenListNumber):
            myMin = listNbr[i]
            for j in range(i + 1, self.lenListNumber):
                if (listNbr[j] < myMin):
                    myMin = listNbr[j]
                    tmp = j
                nbComparisons += 1
            if (myMin != listNbr[i]):
                tmpNum = listNbr[i]
                listNbr[i] = myMin
                listNbr[tmp] = tmpNum
        self.printResult("Selection sort", nbComparisons)
        return nbComparisons

    def insertionSort(self):
            it = 0
            j = 0
            listNumberCopy = self.listNumber.copy()
            for i in range(1, self.lenListNumber):
                j = i
                for j in range(i, 0, -1):
                    it += 1
                    if (listNumberCopy[j - 1] <= listNumberCopy[j]):
                        listNumberCopy[j - 1], listNumberCopy[j] = listNumberCopy[j], listNumberCopy[j - 1]
                    else:
                        break
            print("Insertion sort: %d comparisons" % it)

    def bubbleSort(self):
        tmpNum = 0
        nbComparisons = 0
        listNbr = self.listNumber.copy()

        for i in range(self.lenListNumber - 1):
            for j in range(self.lenListNumber - i - 1):
                nbComparisons += 1
                if (listNbr[j] > listNbr[j + 1]):
                    tmpNum = listNbr[j]
                    listNbr[j] = listNbr[j + 1]
                    listNbr[j + 1] = tmpNum
        self.printResult("Bubble sort", nbComparisons)
        return nbComparisons

    def mergeSort(self):
        listNumberCopy = self.listNumber.copy()
        nbComparisons = self.partitionMerge(listNumberCopy)
        self.printResult("Merge sort", nbComparisons)
        return nbComparisons

    def partitionMerge(self, tab):
        nbComparisons = 0
        if (len(tab) > 1):
            middle = len(tab) // 2
            listNbr = tab.copy()
            left = listNbr[:middle]
            right = listNbr[middle:]

            nbComparisons += self.partitionMerge(left)
            nbComparisons += self.partitionMerge(right)

            i = 0
            j = 0
            k = 0

            while (i < len(left) and j < len(right)):
                nbComparisons += 1
                if (left[i] < right[j]):
                    tab[k] = left[i]
                    i += 1
                else:
                    tab[k] = right[j]
                    j += 1
                k += 1
            while (i < len(left)):
                tab[k] = left[i]
                i += 1
                k += 1
            while (j < len(right)):
                tab[k] = right[j]
                j += 1
                k += 1
        return nbComparisons

    def quickSort(self):
        if (self.lenListNumber == 1):
            self.printResult("Quicksort", 0)
        else:
            listNbr = self.listNumber.copy()
            nbComparisons = self.recursiveQuickSort(listNbr, 0, self.lenListNumber - 1, 0)
            self.printResult("Quicksort", nbComparisons)
    
    # celui la il est partis en couille il marche pas
    # def partitionQuick(self, listNbr, first, last, nbComparisons):
    #     pivot = listNbr[first]
    #     i = last + 1

    #     for j in range(last, first, -1):
    #         print("listNbr[{}] <= pivot || {} <= {}".format(j, listNbr[j], pivot))
    #         if (listNbr[j] >= pivot):
    #             i -= 1
    #             tmpNum = listNbr[i]
    #             listNbr[i] = listNbr[j]
    #             listNbr[j] = tmpNum
    #         nbComparisons += 1
    #         print("LIST FIN BOUCLE = ", listNbr)
    #         print("nbComparisons = ", nbComparisons)
    #     tmpNum = listNbr[i - 1]
    #     listNbr[i - 1] = listNbr[first]
    #     listNbr[first] = tmpNum
    #     print("iiii == ", i)
    #     return i - 1, nbComparisons, listNbr
    
    # def recursiveQuickSort(self, listNbr, first, last, nbComparisons):
    #     print("---- Début Récursive ----")
    #     print("first = {} | last = {}".format(first, last))
    #     print("LIST = ", listNbr)
    #     if (first < last):
    #         pos, nbComparisons, listNbr = self.partitionQuick(listNbr, first, last, nbComparisons)
    #         print("pos = ", pos)
    #         nbComparisons = self.recursiveQuickSort(listNbr, first, pos - 1, nbComparisons)
    #         nbComparisons = self.recursiveQuickSort(listNbr, pos + 1, last, nbComparisons)
    #     print("LIST = ", listNbr)
    #     print("Return nbComparisons = ", nbComparisons)
    #     return nbComparisons


    #Pivot == Last
    # def partitionQuick(self, listNbr, first, last, nbComparisons):
    #     pivot = listNbr[last]
    #     i = first - 1

    #     for j in range(last, first, -1):
    #         if (listNbr[j] <= pivot):
    #             i += 1
    #             tmpNum = listNbr[i]
    #             listNbr[i] = listNbr[j]
    #             listNbr[j] = tmpNum
    #         nbComparisons += 1
    #         print("nbComparisons = ", nbComparisons)
    #     tmpNum = listNbr[i + 1]
    #     listNbr[i + 1] = listNbr[first]
    #     listNbr[first] = tmpNum
    #     return i + 1, nbComparisons, listNbr
    
    # def recursiveQuickSort(self, listNbr, first, last, nbComparisons):
    #     print("---- Début Récursive ----")
    #     print("first = {} | last = {}".format(first, last))
    #     if (first < last):
    #         print("LIST = ", listNbr)
    #         pos, nbComparisons, listNbr = self.partitionQuick(listNbr, first, last, nbComparisons)
    #         nbComparisons = self.recursiveQuickSort(listNbr, first, pos - 1, nbComparisons)
    #         nbComparisons = self.recursiveQuickSort(listNbr, pos + 1, last, nbComparisons)
    #     print("Return nbComparisons = ", nbComparisons)
    #     return nbComparisons

    #PIVOT = FIRST BUT EXCHANGE DIRECTLY WITH LAST
    # def partitionQuick(self, listNbr, first, last, nbComparisons):
    #     pivot = listNbr[first]
    #     j = 0
    #     tmp = 0

    #     for i in range(first + 1, last):
    #         if (listNbr[i] < pivot):
    #             j += 1
    #             listNbr[i], listNbr[j] = listNbr[j], listNbr[i]
    #         nbComparisons += 1
    #     listNbr[first], listNbr[j] = listNbr[j], listNbr[first]
    #     return j - 1, nbComparisons, listNbr
    
    # def recursiveQuickSort(self, listNbr, first, last, nbComparisons):
    #     # print("---- Début Récursive ----")
    #     # print("first = {} | last = {}".format(first, last))
    #     if (first < last):
    #         # print("LIST = ", listNbr)
    #         pos, nbComparisons, listNbr = self.partitionQuick(listNbr, first, last, nbComparisons)
    #         nbComparisons = self.recursiveQuickSort(listNbr, first, pos - 1, nbComparisons)
    #         nbComparisons = self.recursiveQuickSort(listNbr, pos + 1, last, nbComparisons)
    #     # print("Return nbComparisons = ", nbComparisons)
    #     print("LISTNBR = ", listNbr)
    #     return nbComparisons

    #PIVOT == First
    def partitionQuick(self, listNbr, first, last, nbComparisons):
        pivot = listNbr[first]
        i = last + 1

        # nbComparisons += 1
        for j in range(last, first, -1):
            if (listNbr[j] >= pivot):
                i -= 1
                tmpNum = listNbr[i]
                listNbr[i] = listNbr[j]
                listNbr[j] = tmpNum
            nbComparisons += 1
            # print("nbComparisons = ", nbComparisons)
        tmpNum = listNbr[i - 1]
        listNbr[i - 1] = listNbr[first]
        listNbr[first] = tmpNum
        return i - 1, nbComparisons, listNbr
    
    def recursiveQuickSort(self, listNbr, first, last, nbComparisons):
        # print("---- Début Récursive ----")
        # print("first = {} | last = {}".format(first, last))
        if (first < last):
            # print("LIST = ", listNbr)
            pos, nbComparisons, listNbr = self.partitionQuick(listNbr, first, last, nbComparisons)
            nbComparisons = self.recursiveQuickSort(listNbr, first, pos - 1, nbComparisons)
            nbComparisons = self.recursiveQuickSort(listNbr, pos + 1, last, nbComparisons)
        # print("Return nbComparisons = ", nbComparisons)
        return nbComparisons

    # def quickSortAlgo(self, list, lPointer, rPointer):
    #     pivot = list[lPointer]
    #     low = lPointer
    #     high = rPointer
    #     print("low: ", low)
    #     print("high: ", high)
    #     # lPointer = 0
    #     # rPointer = self.lenListNumber - 2
    #     list[low], list[high] = list[high], list[low]
    #     rPointer -= 1
    #     while(lPointer <= rPointer):
    #         print(list)
    #         print("rPointer: ", rPointer)
    #         print("lPointer: ", lPointer)
    #         if (list[lPointer] > pivot or list[rPointer] > pivot):
    #             self.quickSortIt += 1
    #             list[lPointer], list[rPointer] = list[rPointer], list[lPointer]
    #             rPointer -= 1
    #             lPointer += 1
    #     list[high], list[low] = list[low], list[high]
    #     print("rPointer Final: ", rPointer)
    #     # self.quickSortAlgo(list, 0, rPointer)
    #     # self.quickSortAlgo(list, rPointer, self.lenListNumber - 1)
    #     print("list: ", list)


    # Version Tony
    # def quickSortAlgo(self, list, first, last):
    #     print("first: ", first)
    #     print("last: ", last)
    #     pivot = list[first]
    #     print("pivot: ", pivot)
    #     list[first], list[last] = list[last], list[first]
    #     i = -1
    #     j = 0
    #     while (j < last):
    #         if (list[j] <= pivot):
    #             self.quickSortIt += 1
    #             i += 1
    #             list[i], list[j] = list[j], list[i]
    #         j += 1
    #     list[last], list[i + 1] = list[i + 1], list[last]
    #     # print(list)
    #     if (i > first):
    #         print("LEFT")
    #         self.quickSortAlgo(list, first, i)
    #     if (i + 2 < last):
    #         print("RIGHT")
    #         self.quickSortAlgo(list, i + 2, last)



    # def quickSort(self):
    #     listNumberCopy = self.listNumber.copy()
    #     self.quickSortAlgo(listNumberCopy, 0 ,self.lenListNumber - 1)
    #     self.printResult("quick sort", self.quickSortIt)
    #     print(listNumberCopy)


    def printResult(self, nameSort, nbComparisons):
        if (nbComparisons == 1):
            print("{}: {} comparison".format(nameSort, nbComparisons))
        else:
            print("{}: {} comparisons".format(nameSort, nbComparisons))

def main():
    if (len(sys.argv) == 2 and sys.argv[1] == "-h"):
        printUsage()
    elif (len(sys.argv) == 2):
        dannon = Dannon(sys.argv[1])
        dannon.selectSort()
        dannon.insertionSort()
        dannon.bubbleSort()
        dannon.quickSort()
        dannon.mergeSort()
    else:
        raise ValueError("Error: please look -h for using 206eutrinos")

if __name__ == "__main__":
    try:
        main()
        sys.exit(0)
    except (ValueError, ZeroDivisionError, OSError) as ve:
        print(ve)
        sys.exit(84)