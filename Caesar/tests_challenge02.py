import unittest
import sys
import io
from contextlib import redirect_stdout, redirect_stderr

from source02 import Challenge02

class ProjectTest(unittest.TestCase):
    def testFixedXor(self):
        newChallenge02 = Challenge02()
        f = io.StringIO()
        with redirect_stdout(f):
            newChallenge02.run("exemple/input02.txt")
        stdoutStr = f.getvalue()
        self.assertEqual("1118000F0725001A\n", stdoutStr)

    def testChallen02WithNoExistFile(self):
        newChallenge02 = Challenge02()
        with self.assertRaises(FileNotFoundError) : newChallenge02.run("exemple/no_exist_file.txt")

    def testChallen01WithEmptyFile(self):
        newChallenge02 = Challenge02()
        with self.assertRaises(ValueError) : newChallenge02.run("exemple/input01_empty.txt")

    def testChallen01WithNoRightFile(self):
        newChallenge02 = Challenge02()
        with self.assertRaises(PermissionError) : newChallenge02.run("exemple/input01_no_right.txt")

if __name__ == '__main__':
    unittest.main()


