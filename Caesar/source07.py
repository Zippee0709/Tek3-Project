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


class Challenge07:
    def __init__(self):
        self.key = None
        self.message = None
        self.cipherText = None
        self.result = None

    def run(self, file):
        self.readFile(file)
        self.key = bytes.fromhex(self.key)
        self.message = b64decode(self.message)
        self.cipherText = AES.new(self.key, AES.MODE_ECB)
        self.result = self.cipherText.decrypt(self.message)
        # print(type(self.result))
        # print(self.result.decode('utf-8'))
        print(b64encode(self.result).decode('utf-8'))

    def decryptECB(self):
        cipher = AES.new(self.key, AES.MODE_ECB)
        plainText = cipher.decrypt(self.cipherText)
        return plainText

    def readFile(self, file):
        try :
            myFile = open(file, "r")
            self.key = myFile.readline().replace("\n", "")
            self.message = myFile.readline().replace("\n", "")
            myFile.close()
            if (self.key == None or self.message == None):
                raise ValueError("Error: Invalid File!")
            if (len(self.key) == 0 or len(self.message) == 0):
                raise ValueError("Error: Invalid File!")
            if (len(self.key) % 16 != 0):
                raise ValueError("Error : Invalid Key")
        except:
            raise Exception("Error : Invalid file !")

def main():
    if (len(sys.argv) == 2):
        myChallenge07 = Challenge07()
        myChallenge07.run(sys.argv[1])
    else:
        raise ValueError("Error: invalid inputs !")

if __name__ == "__main__":
    try:
        main()
    except (ValueError, ZeroDivisionError, OSError) as ve:
        print(ve)
        sys.exit(84)