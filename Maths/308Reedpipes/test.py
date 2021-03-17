#!/usr/bin/env python3

from sys import argv, stderr, exit
from math import factorial

# Do not modify this class, his default comportement is necessary for the program
class   BadArgumentError(Exception):
    def __init__(self, message, errors = "BadArgumentError"):
        super().__init__(message)
        self.errors = errors

# Default object class
class   Spline():
    """ Definition of Spline class """
    result = []
    ordinate = []
    abscissa = [0, 5, 10, 15, 20]
    vector = [0, None, None, None, 0]
    var = {'r0': 0, 'r5': 0, 'r10': 0, 'r15': 0, 'r20': 0, 'n': 0}    
    def __init__(self, argument, total=0):
        """ Initialise Separation's instance and check little errors """
        for arg, i in zip(self.var, range(1, len(argument))):
            if float(argument[i]) <= 0:
                raise BadArgumentError("'{}' must be positive (is {})".format(arg, argument[i]))
            self.var[arg] = float(argument[i])
            if self.var[arg] is not self.var['n']:
                self.ordinate.append(self.var[arg])

    def computeValue(self):
        """ Calculate all printable value required by printValue method """
        A = 6 * (self.var['r10'] - 2 * self.var['r5'] + self.var['r0']) / 50
        B = 6 * (self.var['r15'] - 2 * self.var['r10'] + self.var['r5']) / 50
        C = 6 * (self.var['r20'] - 2 * self.var['r15'] + self.var['r10']) / 50
        print("A = ", A)
        print("B = ", B)
        print("C = ", C)
        self.vector[2] = (B - (A + C) / 4) * 4 / 7
        self.vector[1] = A / 2 - 0.25 * self.vector[2]
        self.vector[3] = C / 2 - 0.25 * self.vector[2]
        print(self.vector)
        for d in range(int(self.var['n'])):
            X = 20 / (self.var['n'] - 1) * d
            i = int((X - 0.01) / 5) + 1
            result = (- self.vector[i - 1] / 30 * pow(X - self.abscissa[i], 3)
                      + self.vector[i] / 30 * pow(X - self.abscissa[i - 1], 3)
                      - (self.ordinate[i - 1] / 5 - 5 / 6 * self.vector[i - 1])
                      * (X - self.abscissa[i])
                      + (self.ordinate[i] / 5 - 5 / 6 * self.vector[i])
                      * (X - self.abscissa[i - 1]))
            # a = - self.vector[i - 1] / 30 * pow(X - self.abscissa[i], 3)
            # b = self.vector[i] / 30 * pow(X - self.abscissa[i - 1], 3)
            # c = (self.ordinate[i - 1] / 5 - 5 / 6 * self.vector[i - 1])
            # d = (X - self.abscissa[i])
            # e = (self.ordinate[i] / 5 - 5 / 6 * self.vector[i])
            # f = (X - self.abscissa[i - 1])
            # res = a + b - c * d - e * f
            self.result.append(result)

    def printValue(self):
        """ Print all computed value into the tab """
        print("vector result: [{:.1f}, {:.1f}, {:.1f}, {:.1f}, {:.1f}]".format(
            self.vector[0] if round(self.vector[0], 1) != 0 else 0,
            self.vector[1] if round(self.vector[1], 1) != 0 else 0,
            self.vector[2] if round(self.vector[2], 1) != 0 else 0,
            self.vector[3] if round(self.vector[3], 1) != 0 else 0,
            self.vector[4] if round(self.vector[4], 1) != 0 else 0))
        for i in range(int(self.var['n'])):
            print("abscissa: {:.1f} cm\tradius: {:.1f} cm".format(
                20 / (self.var['n'] - 1) * i, self.result[i]))

# Do not put more information in this function, it's must be clearer as possible
def     main():
    """ Main function who perform program's core action like arguments resolution """
    if len(argv) is not 7:
        raise BadArgumentError("Usage: ./308reedpipes r0 r5 r10 r15 r20 n")
    obj = Spline(argv)
    obj.computeValue()
    obj.printValue()

# Don't touch at this except if u don't worry of problems
if __name__ == "__main__":
    try:
        main()
    except BaseException as error:
        stderr.write(str(type(error).__name__) + ": {}\n".format(error))
        exit(84)