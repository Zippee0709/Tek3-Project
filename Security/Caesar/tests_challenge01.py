import unittest
import sys
import io
from contextlib import redirect_stdout, redirect_stderr

from source01 import Challenge01

class ProjectTest(unittest.TestCase):
    def testConvert2CharacterHexToBase64(self):
        newChallenge01 = Challenge01()
        f = io.StringIO()
        with redirect_stdout(f):
            newChallenge01.run("exemple/input01.txt")
        stdoutStr = f.getvalue()
        self.assertEqual("Sw==\n", stdoutStr)

    def testConvert2CharacterHexToBase64WithNewLine(self):
        newChallenge01 = Challenge01()
        f = io.StringIO()
        with redirect_stdout(f):
            newChallenge01.run("exemple/input01_with_new_line.txt")
        stdoutStr = f.getvalue()
        self.assertEqual("Sw==\n", stdoutStr)

    def testChallen01WithNoExistFile(self):
        newChallenge01 = Challenge01()
        with self.assertRaises(FileNotFoundError) : newChallenge01.run("exemple/no_exist_file.txt")

    def testChallen01WithEmptyFile(self):
        newChallenge01 = Challenge01()
        with self.assertRaises(ValueError) : newChallenge01.run("exemple/input01_empty.txt")

    def testChallen01WithNoRightFile(self):
        newChallenge01 = Challenge01()
        with self.assertRaises(PermissionError) : newChallenge01.run("exemple/input01_no_right.txt")

    def testChallen01With1CharacterHexString(self):
        newChallenge01 = Challenge01()
        with self.assertRaises(ValueError) : newChallenge01.run("exemple/input01_1Char.txt")

    def testChallen01With27CharacterHexString(self):
        newChallenge01 = Challenge01()
        with self.assertRaises(ValueError) : newChallenge01.run("exemple/input01_27Char.txt")

    def testChallen01With49CharacterHexString(self):
        newChallenge01 = Challenge01()
        with self.assertRaises(ValueError) : newChallenge01.run("exemple/input01_49Char.txt")

if __name__ == '__main__':
    unittest.main()


