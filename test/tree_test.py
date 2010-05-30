import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.stxt.tree import *

class UnitTest(unittest.TestCase):
    def setUp(self):
        d = Tree('doc')
        d.append(Tree('question', 'q0', name='q0'))
        d.append(Tree('question', 'q1', name='q1'))
        d.append(Tree('example', 'e0', name='e0'))
        d.append(Tree('question', 'q2', name='q2'))
        d.append(Tree('example', 'e1', name='e1'))

        self.tree = d

        r'''
    2
   / \
  7   5  
 / \   \
2  6    9
  / \   /
 5  11  4
'''
        self.btree = Tuple2BTree(2,(7,2,(6, 5, 11)), (5, None, (9, 4)))
        
    def testGetDirectChild(self):
        t = self.tree
        self.assertEqual('q2', t[3].value)

    def testOrderPath(self):
        pass

    def testPath(self):
        pass 

    def testSibling(self):
        d = self.tree

        self.assertEqual(3, len(d.children_in_type('question')))
        self.assertEqual(2, len(d.children_in_type('example')))

        q1 = d.child(1)
        self.assertEqual('q2', q1.brother(2).value)
        self.assertEqual('e0', q1.sibling(2).value)
        self.assertEqual(1, q1.order())

    def testNameTable(self):
        r = Tree('book', 'book value', name='book')
        r.append(Tree('child', 'child value', name='child'))

        self.assertEqual('book value', r.find_by_name('book').value)
        self.assertEqual('child value', r.find_by_name('child').value)

    def testAddress(self):
        t = Tree('book', 'fhopecc book', name='fhopecc')
        c = Tree('sect', 'section first', name='sect1')
        d = Tree('sect', 'section second', name='sect1')
        t.append(c)
        c.append(d)

        t.root().make_name_table()

        self.assertEqual('fhopecc book', c.get('fhopecc').value)
        self.assertEqual('section first', t.get('sect1').value)

    def testPrevious(self):
        r = Tree('book', 'book value', name='book')
        r.append(Tree('sect1', 'sect1 value', name='child1'))
        r.append(Tree('sect2', 'sect2 value', name='child2'))
        r.append(Tree('sect1', 'sect1 value', name='child3'))
        sect1s = [c for c in r.children if c.type == 'sect1']
        child3 = r.find_by_name('child3')
        self.assertEqual(1, sect1s.index(child3))

    def testPreorder(self):
        pass
    
    def testDFS(self):
        btree = self.btree
        seq = [n.value for n in btree.dfs()]
        self.assertEqual([2, 5, 9, 4, 7, 6, 11, 5, 2], seq)
        
    def testTuple2BTree(self):
        self.assertRaises(ValueError, Tuple2BTree, None,2,3)

        t = Tuple2BTree(2)                    
        self.assertEqual('root', t.type)
        self.assertEqual(2, t.value)

        t = Tuple2BTree(2,(7,2,6))                    
        self.assertEqual('root', t.type)
        self.assertEqual(2, t.value)
        self.assertEqual('left', t[0].type)
        self.assertEqual(7, t[0].value)
        
        self.assertEqual('right', t[0][1].type)
        self.assertEqual(6, t[0][1].value)
    
    def testReferenceNode(self):
        tree = self.tree
        ref = ReferenceNode('e1', 'label')
        tree.append(ref)

        self.assertEqual('reference', ref.type)
        self.assertEqual('e1', ref.address)
        self.assertEqual('label', ref.label)
        tree.root().make_name_table()
        e1 = tree.get('e1')

        self.assertEqual(e1, ref.reftree())

if __name__ == '__main__':
    unittest.main()
