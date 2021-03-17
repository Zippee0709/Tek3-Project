import unittest
import sys
import io
from contextlib import redirect_stdout, redirect_stderr

from source04 import Crypto

class ProjectTest(unittest.TestCase):
    def testConvert2CharacterHexToBase64(self):
        newCrypto = Crypto()
        f = io.StringIO()
        with redirect_stdout(f):
            newCrypto.run("exemple/input04.txt")
        stdoutStr = f.getvalue()
        self.assertEqual("6 42\n", stdoutStr)

    def testChallen01WithNoExistFile(self):
        newCrypto = Crypto()
        with self.assertRaises(Exception) : newCrypto.run("exemple/no_exist_file.txt")

    def testChallen01WithEmptyFile(self):
        newCrypto = Crypto()
        with self.assertRaises(Exception) : newCrypto.run("exemple/input01_empty.txt")

    def testChallen01WithNoRightFile(self):
        newCrypto = Crypto()
        with self.assertRaises(Exception) : newCrypto.run("exemple/input01_no_right.txt")

if __name__ == '__main__':
    unittest.main()


