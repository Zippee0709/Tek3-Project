import unittest
import sys

from dannon_301 import Dannon

class ProjectTest(unittest.TestCase):
    def testSelectSort1(self):
        print("------------ Test file.txt ------------")
        dannon = Dannon("file.txt")
        self.assertEqual(6, dannon.selectSort())

    def testSelectSort2(self):
        print("------------ Test file2.txt ------------")
        dannon = Dannon("file2.txt")
        self.assertEqual(210, dannon.selectSort())

    def testInsertionSort1(self):
        print("------------ Test file.txt ------------")
        dannon = Dannon("file.txt")
        self.assertEqual(4, dannon.insertionSort())

    def testInsertionSort2(self):
        print("------------ Test file2.txt ------------")
        dannon = Dannon("file2.txt")
        self.assertEqual(125, dannon.insertionSort())

    def testBubbleSort1(self):
        print("------------ Test file.txt ------------")
        dannon = Dannon("file.txt")
        self.assertEqual(6, dannon.bubbleSort())

    def testBubbleSort2(self):
        print("------------ Test file2.txt ------------")
        dannon = Dannon("file2.txt")
        self.assertEqual(210, dannon.bubbleSort())

    def testFusionSort1(self):
        print("------------ Test file.txt ------------")
        dannon = Dannon("file.txt")
        self.assertEqual(5, dannon.fusionSort())

    def testFusionSort2(self):
        print("------------ Test file2.txt ------------")
        dannon = Dannon("file2.txt")
        self.assertEqual(67, dannon.fusionSort())

    # def testQuickSort1(self):
    #     print("------------ Test file.txt ------------")
    #     dannon = Dannon("file.txt")
    #     self.assertEqual(4, dannon.insertionSort())

    # def testQuickSort2(self):
    #     print("------------ Test file2.txt ------------")
    #     dannon = Dannon("file2.txt")
    #     self.assertEqual(80, dannon.insertionSort())

if __name__ == '__main__':
    unittest.main()
