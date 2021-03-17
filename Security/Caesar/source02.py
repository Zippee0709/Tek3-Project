#!/usr/bin/env python3
##
## EPITECH PROJECT, 2021
## B-SEC-500-PAR-5-1-caesar-zhiwen.wang
## File description:
## source02
##

import sys

class Challenge02:
    def __init__(self):
        self.file = None

    def run(self, file):
        try:
            file = open(file, "r")
            firstLine = file.readline().replace("\n", "")
            secondLine = file.readline().replace("\n", "")
            file.close()
            if len(firstLine) != len(secondLine):
                raise ValueError("Line length different")
            elif (len(firstLine) == 0 or len(secondLine) == 0):
                raise ValueError("Error : the file is empty")
            firstHex = bytes.fromhex(firstLine)
            secondHex = bytes.fromhex(secondLine)
            res = []
            for i in range(len(firstHex)):
                firstChar = int(firstHex[i])
                secondChar = int(secondHex[i])
                res.append(firstChar ^ secondChar)
            ls = [f"{i:0>2x}" for i in res]
            print(''.join(ls).upper())

        except (IOError, ValueError, PermissionError, FileNotFoundError) as e:
            raise e

def main():
    if (len(sys.argv) == 2):
        myChaChallenge02 = Challenge02()
        myChaChallenge02.run(sys.argv[1])
    else:
        raise ValueError("Error: invalid inputs !")

if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, PermissionError, FileNotFoundError) as ve:
        print(ve)
        sys.exit(84)