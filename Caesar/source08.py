#!/usr/bin/env python3
##
## EPITECH PROJECT, 2021
## B-SEC-500-PAR-5-1-caesar-zhiwen.wang
## File description:
## source07
##

import sys
from base64 import b64encode, b64decode
from Crypto.Cipher import AES


class Challenge08:
    def __init__(self):
        self.fileContents = None
        self.blockSize = 16
        self.res = None

    def run(self, file):
        self.readFile(file)
        self.detectECB()

    def readFile(self, file):
        try :
            myFile = open(file, "r")
            self.fileContents = [line.strip('\n') for line in myFile.readlines()]
            myFile.close()
            if (self.fileContents == None or len(self.fileContents) == 0):
                raise ValueError("Error : Invalid file contents")
        except (ValueError, OSError, PermissionError, FileNotFoundError) as ve:
            raise ve

    def detectECB(self):
        self.res = list()
        nbOfLine = 1

        for line in self.fileContents:
            tmp = b64decode(line)
            nbReapeat = self.countNbOfReapeat(tmp)
            self.res.append([nbOfLine, nbReapeat])
            nbOfLine += 1
        self.res = sorted(self.res, key = lambda x: x[1], reverse=True)[0]
        print(self.res[0])

    def countNbOfReapeat(self, tmp):
        chunks = [tmp[i:i + self.blockSize] for i in range(0, len(tmp), self.blockSize)]
        nbReapeat = len(tmp) - len(set(chunks))
        # print("reapeat = {}\tmsg = {}".format(nbReapeat, chunks))
        return nbReapeat

def main():
    if (len(sys.argv) == 2):
        myChallenge08 = Challenge08()
        myChallenge08.run(sys.argv[1])
    else:
        raise ValueError("Error: invalid inputs !")

if __name__ == "__main__":
    try:
        main()
    except (OSError, PermissionError, FileNotFoundError) as ve:
        print(ve)
        sys.exit(84)