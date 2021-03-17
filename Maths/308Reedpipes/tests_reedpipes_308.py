#!/usr/bin/env python3

import unittest
import sys
import io

from reedpipes_308 import Reedpipes
from contextlib import redirect_stdout, redirect_stderr

class ProjectTest(unittest.TestCase):
    def testBadArgv(self):
        newReedpipes = Reedpipes()
        f = io.StringIO()
        with redirect_stdout(f):
            newReedpipes.run(["1.5", "2"])
        stdoutStr = f.getvalue()
        self.assertEqual("Error: please look -h for using 308Reedpipes\n", stdoutStr)

if __name__ == '__main__':
    unittest.main()
