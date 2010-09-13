# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from datetime import date
from datetime_iterator import datetimeIterator

class UnitTest(unittest.TestCase):

    def testDatetimeIterator(self):
        i = datetimeIterator(date(2010, 9, 1), date(2010, 9, 2))

        d = i.next()
        self.assertEqual(date(2010, 9, 1), d)

        d = i.next()
        self.assertEqual(date(2010, 9, 2), d)
