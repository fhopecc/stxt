import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
import testhelper
import unittest
import rest

class TestRest(unittest.TestCase):
  def testresource(self):
    fn = 'app/model/resource.rb'
    self.assertEqual('resource',rest.resource(fn))

if __name__ == '__main__':
    unittest.main()

