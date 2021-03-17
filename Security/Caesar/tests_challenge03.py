import unittest
import sys
import io
from contextlib import redirect_stdout, redirect_stderr

from source03 import Crypto

class ProjectTest(unittest.TestCase):
    def testSubjectEncryptText(self):
        newCrypto = Crypto()
        f = io.StringIO()
        with redirect_stdout(f):
            newCrypto.run("exemple/input03.txt")
        stdoutStr = f.getvalue()
        self.assertEqual("42\n", stdoutStr)

    def testEncryptText(self):
        newCrypto = Crypto()
        f = io.StringIO()
        with redirect_stdout(f):
            newCrypto.run("exemple/input03_2.txt")
        stdoutStr = f.getvalue()
        self.assertEqual("58\n", stdoutStr)

    def testChallen01WithNoExistFile(self):
        newCrypto = Crypto()
        with self.assertRaises(ValueError) : newCrypto.run("exemple/no_exist_file.txt")

    def testChallen01WithEmptyFile(self):
        newCrypto = Crypto()
        with self.assertRaises(ValueError) : newCrypto.run("exemple/input01_empty.txt")

    def testChallen01WithNoRightFile(self):
        newCrypto = Crypto()
        with self.assertRaises(ValueError) : newCrypto.run("exemple/input01_no_right.txt")

if __name__ == '__main__':
    unittest.main()


