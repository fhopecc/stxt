import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.tree import *
class UnitTest(unittest.TestCase):
    def testTreeConstructor(self):
        root = Tree('value', 
                {'x' : 'v1', 'y' : 'v2'}, 
                trunk='v3', 
                branch='v4')
        self.assertEqual('value', root())
        self.assertEqual('v1', root['x']())
        self.assertEqual('v2', root['y']())
        self.assertEqual('v3', root.trunk())
        self.assertEqual('v4', root.branch())

    def testAttribute(self):
        root = Tree('value')
        root.dir = 'v1'
        self.assertEqual('v1', root.dir())
    
    def testTraverse(self):                                            
        root = Tree('value', 
                {'x' : 'v1', 'y' : 'v2'}, 
                trunk='v3', 
                branch='v4')
        i = root('traverse')
        seq = i.next()
        n = seq[0]
        self.assertEqual([], n)
        n = seq[1]
        self.assertEqual('value', n())

        seq = i.next()
        n = seq[0]
        self.assertEqual(['branch'], n)
        n = seq[1]
        self.assertEqual('v4', n())

if __name__ == '__main__':
    unittest.main()
