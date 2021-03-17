#!/usr/bin/env python3
##
## EPITECH PROJECT, 2021
## B-SEC-500-PAR-5-1-caesar-zhiwen.wang
## File description:
## source05
##

import sys
import base64

class Challenge03:
    def __init__(self):
        self.byteString = None

    def breakSingleByteXor(self, ciphertext):
        self.byteString = ciphertext
        return self.decryptAllMsg()

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
        # self.sortResult(res)
        return sorted(res, key = lambda x : x['score'], reverse = True)[0]

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
        tmp = bytes([bestResult['key']]).hex()
        print(tmp)

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

def hammingDistance(x, y):
    distance = 0
    limit = len(x) if len(x) < len(y) else len(y)
    for i in range(0, limit):
        binX = bin(x[i])[2:]
        binY = bin(y[i])[2:]
        if (len(binX) < len(binY)):
            binX = binX.zfill(len(binY))
        elif (len(binX) > len(binY)):
            binY = binY.zfill(len(binX))
        for j in range(0, len(binX)):
            if (not(binX[j] == binY[j])):
                distance += 1
    return distance

# def calculate_hamming_distance(input_bytes_1, input_bytes_2):
#     """Finds and returns the Hamming distance (number of differing 
#     bits) between two byte-strings
#     """
#     hamming_distance = 0
#     for b1, b2 in zip(input_bytes_1, input_bytes_2):
#         difference = b1 ^ b2

#         # Count the number of differences ('1's) and add to the hamming distance
#         hamming_distance += sum([1 for bit in bin(difference) if bit == '1'])
#     return hamming_distance
    
def breakRepeatingKeyXor(ciphertext):
    averageDistance = []
    for keysize in range(5, 41):
        distances = []
        chunks = []
        counter = 0
        for i in range(0, len(ciphertext), keysize):
            chunks += [ciphertext[i:i + keysize]]

        while counter < len(chunks) - 1:
            firstChunk = chunks[counter]
            secondChunk = chunks[counter + 1]
            # print("firstChunk: ", firstChunk)
            # print("secondChunk: ", secondChunk)
            # sys.exit(55)
            distances.append(hammingDistance(firstChunk, secondChunk))
            counter += 1
        averageDistance.append({"key": keysize, "avgDistance": sum(distances) / len(distances)})

    possible_plaintext = []
    key = b''
    possible_key_length = sorted(averageDistance, key = lambda i: i["avgDistance"])[0]["key"]
    c3 = Challenge03()
    for i in range(possible_key_length):
        block = b''
        for j in range(i, len(ciphertext), possible_key_length):
            block += bytes([ciphertext[j]])
        key += bytes([c3.breakSingleByteXor(block)['key']])
    possible_plaintext.append((repeatKeyXor(ciphertext, key), key))
    print(possible_plaintext)
    return max(possible_plaintext, key=lambda x: c3.calculateScore(x[0]))

def main():
    if len(sys.argv) <= 1:
        raise ValueError("Error: The input can't be empty")
    try:
        # with open(sys.argv[1]) as input_file:
        #     byteEncryptMessage = base64.b64decode(input_file.read())
        # print(byteEncryptMessage)
        print("start okok")
        file = open(sys.argv[1], "r")
        byteEncryptMessage = bytes.fromhex(file.readline())
        # print(byteEncryptMessage)
        res, key = breakRepeatingKeyXor(byteEncryptMessage)
        print("key = {}\nres = {}".format(key, res))
        print(key.hex())
    except (IOError, ValueError) as e:
        raise ValueError("Error: ", e)

if __name__ == "__main__":
    try:
        main()
    except (ValueError, ZeroDivisionError, OSError) as ve:
        print(ve)
        sys.exit(84)