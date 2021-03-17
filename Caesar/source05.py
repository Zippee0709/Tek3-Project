#!/usr/bin/env python3
##
## EPITECH PROJECT, 2021
## B-SEC-500-PAR-5-1-caesar-zhiwen.wang
## File description:
## source05
##

import sys

def repeatKeyXor(keyBin, messageBin):
    outputBytes = b''
    index = 0
    for byte in messageBin:
        outputBytes += bytes([byte ^ keyBin[index]])
        if index + 1 < len(keyBin):
            index += 1
        else: 
            index = 0
    return outputBytes

def main():
    if len(sys.argv) <= 1:
        raise ValueError("Error: The input can't be empty")

    file = open(sys.argv[1], "r")

    hexEncodedKey = bytearray.fromhex(file.readline().replace("\n", ""))
    hexMessage = bytearray.fromhex(file.readline().replace("\n", ""))

    res = repeatKeyXor(hexEncodedKey, hexMessage)
    print(res.hex().upper())

if __name__ == "__main__":
    try:
        main()
    except (ValueError, ZeroDivisionError, OSError) as ve:
        print(ve)
        sys.exit(84)