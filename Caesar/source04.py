#!/usr/bin/env python3
##
## EPITECH PROJECT, 2021
## B-SEC-500-PAR-5-1-caesar-zhiwen.wang
## File description:
## source04
##

import sys

class Crypto:
    def __init__(self):
        self.line = None
        self.lines = None
        self.byteString = None
        self.resultDict = None

    def run(self, file):
        self.readFile(file)
        self.resultDict = dict()
        i = 0

        for self.line in self.lines:
            self.line = self.line.replace("\n", "")
            self.convertHexToBytes()
            bestResult = self.decryptAllMsg()
            self.resultDict[i] = bestResult
            i += 1

        sortedBestResult = sorted(self.resultDict, key = lambda x: self.resultDict[x]['score'], reverse = True)[0]
        # print("self.resultDict[{}] = {}".format(sortedBestResult, self.resultDict[sortedBestResult]["key"]))
        finaleResult = bytes([self.resultDict[sortedBestResult]["key"]]).hex()
        print(sortedBestResult + 1, end=" ")
        print(finaleResult)
        # bestResult = sorted(self.resultDict, key = lambda x : x['score'], reverse = True)[0]
        # print("bestResult = ", bestResult)

    def readFile(self, file):
        try :
            myFile = open(file, "r")
            self.lines = myFile.readlines()
            myFile.close()
            if (len(self.lines) == 0):
                raise ValueError("Error: File empty !")
        except:
            raise Exception("Error : Invalid read file ! ")

    def convertHexToBytes(self):
        self.byteString = bytes.fromhex(self.line)

    def xorCipher(self, key):
        res = b''
        for byte in self.byteString:
            res += bytes([byte ^ key])
        return res

    def decryptAllMsg(self):
        res = []

        for i in range (256):
            decryptedMsg = self.xorCipher(i)
            scoreOfMsg = self.calculateScore(decryptedMsg)
            data = {
                'message' : decryptedMsg,
                'score' : scoreOfMsg,
                'key' : i
            }
            res.append(data)
        return self.sortResult(res)

    ## Frequency divide by 100
    def calculateScore(self, decryptedMsg):
        englishLettersFrequency = {
            'a': .08167, 'b': .01492, 'c': .02782, 'd': .04253,
            'e': .12702, 'f': .02228, 'g': .02015, 'h': .06094,
            'i': .06094, 'j': .00153, 'k': .00772, 'l': .04025,
            'm': .02406, 'n': .06749, 'o': .07507, 'p': .01929,
            'q': .00095, 'r': .05987, 's': .06327, 't': .09056,
            'u': .02758, 'v': .00978, 'w': .02360, 'x': .00150,
            'y': .01974, 'z': .00074, ' ': .13000
        }
        res = []
        for byte in decryptedMsg.lower():
            res.append(englishLettersFrequency.get(chr(byte), 0))
        return sum(res)

    def sortResult(self, res):
        bestResult = sorted(res, key = lambda x : x['score'], reverse = True)[0]
        # print("best result = ", bestResult)
        # tmp = bytes([bestResult['key']]).hex()
        # print(tmp)
        return bestResult

    def printRes(self, res):
        for elem in res:
            print(elem)


def main():
    if (len(sys.argv) == 2):
        myCrypto = Crypto()
        myCrypto.run(sys.argv[1])
    else:
        raise ValueError("Error: invalid inputs !")

if __name__ == "__main__":
    try:
        main()
    except (ValueError, ZeroDivisionError, OSError) as ve:
        print(ve)
        sys.exit(84)