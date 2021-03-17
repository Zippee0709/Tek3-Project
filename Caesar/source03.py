#!/usr/bin/env python3
##
## EPITECH PROJECT, 2021
## B-SEC-500-PAR-5-1-caesar-zhiwen.wang
## File description:
## source03
##

import sys

class Crypto:
    def __init__(self):
        self.line = None
        self.byteString = None
        self.englishLettersFrequency = None

    def run(self, file):
        self.readFile(file)
        self.convertHexToBytes()
        self.decryptAllMsg()
    
    def readFile(self, file):
        try :
            myFile = open(file, "r")
            self.line = myFile.readline().replace("\n", "")
            myFile.close()
            if (len(self.line) == 0):
                raise ValueError("Error: File empty !")
        except:
            raise ValueError("Error : Invalid read file ! ")

    def convertHexToBytes(self):
        self.byteString = bytes.fromhex(self.line)

    def xorCipher(self, key):
        res = b''
        for byte in self.byteString:
            res += bytes([byte ^ key])
        return res

    def decryptAllMsg(self):
        res = []
        self.calculateNewLetterFrequency()

        for i in range (256):
            decryptedMsg = self.xorCipher(i)
            scoreOfMsg = self.calculateScore(decryptedMsg)
            data = {
                'message' : decryptedMsg,
                'score' : scoreOfMsg,
                'key' : i
            }
            res.append(data)
        self.sortResult(res)

    def calculateNewLetterFrequency(self):
        ## Frequency divide by 100
        self.englishLettersFrequency = {
            'a': 8.2389258,    'b': 1.5051398,    'c': 2.8065007,    'd': 4.2904556,
            'e': 12.813865,    'f': 2.2476217,    'g': 2.0327458,    'h': 6.1476691,
            'i': 6.1476691,    'j': 0.1543474,    'k': 0.7787989,    'l': 4.0604477,
            'm': 2.4271893,    'n': 6.8084376,    'o': 7.5731132,    'p': 1.9459884,
            'q': 0.0958366,    'r': 6.0397268,    's': 6.3827211,    't': 9.1357551,
            'u': 2.7822893,    'v': 0.9866131,    'w': 2.3807842,    'x': 0.1513210,
            'y': 1.9913847,    'z': 0.0746517,    ' ': 13.000000
        }
        for elem in self.englishLettersFrequency:
            self.englishLettersFrequency[elem] = self.englishLettersFrequency[elem] / 100

    def calculateScore(self, decryptedMsg):
        res = []
        for byte in decryptedMsg.lower():
            res.append(self.englishLettersFrequency.get(chr(byte), 0))
        return sum(res)

    def sortResult(self, res):
        bestResult = sorted(res, key = lambda x : x['score'], reverse = True)[0]
        print(bytes([bestResult['key']]).hex().upper())

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