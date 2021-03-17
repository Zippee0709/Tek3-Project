#!/usr/bin/env python3
##
## EPITECH PROJECT, 2021
## B-SEC-500-PAR-5-1-caesar-zhiwen.wang
## File description:
## source01
##

from base64 import b64encode, b64decode
import sys

class Challenge01:
    def __init__(self):
        self.line = None
        self.file = None
        self.b64 = None

    def readFile(self, file):
        try:
            self.file = open(file, 'r')
            self.line = self.file.read().replace("\n", "")
            self.file.close()
            lenLine = len(self.line)
            if (lenLine == 0):
                raise ValueError("Error : The file is empty.")
            if (lenLine != 2 and lenLine != 8 and lenLine != 64 and lenLine != 256 and lenLine != 1024 and lenLine != 2048):
                raise ValueError("Error : Invalid hex string.")
        except (ValueError, IOError) as e:
            raise e

    def convertFile(self):
        try : 
            self.b64 = b64encode(bytes.fromhex(self.line)).decode()
        except:
            raise ValueError("Error : cannot convert hex to base64")
    
    def printb64(self):
        print(self.b64)
    
    def run(self, file):
        self.readFile(file)
        self.convertFile()
        self.printb64()

def main():
    if (len(sys.argv) == 2):
        myChallenge01 = Challenge01()
        myChallenge01.run(sys.argv[1])
    else:
        raise ValueError("Error: invalid inputs !")

if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, IOError) as ve:
        print(ve)
        sys.exit(84)