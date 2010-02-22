# coding=utf8
import sys, unittest

class Tree(object):
    def __init__(self, type, value='', title='', name=''):
        self.type, self.value, self.title, self.name = type, value, title, name
        self.parent, self.children = None, []
        self.number = None         # It's section number
        self.occurence = None    # It's table number
        self.name_table = None    

    def __str__(self):
        m = "%s:\n[\n%s\n]" % (self.type, self.value)
        for c in self.children:
            m+=str(c)
        return m

    def __repr__(self):
        return str(self)

    # OK to rename __setattr__
	def setattr(self, attr_name, value):
		# #1
		if attr_name in self.__used_names:
			raise TreeAttrExc(_("Attribute name '%s' is reserved" % attr_name))
		try:
			# If self.attribute exists
			existed = getattr(self, attr_name)
			if isinstance(value, Tree):
				subtree = value
				self.__dict__[attr_name] = subtree
				# Replace the node directly
			else:
				# self.__dict__[attr_name]._Tree__node_value = value
				# This will lead to raise TreeExc at #1, because the setattr operation of 
				#	'self.__dict__[attr_name].attribute = value' has been affected by self.__setattr__()
				subtree = existed
				subtree.__dict__['_Tree__node_value'] = value
				# Only replace the node value
		except AttributeError:
		# if self.attribute does not exists, assign it a EMPTY node
			if isinstance(value, Tree):
				subtree = value
				self.__dict__[attr_name] = subtree
			else:
				self.__dict__[attr_name] = Tree(value)

    def append(self, *nodes):
        for n in nodes: 
            n.parent = self
            self.children.append(n)
        return self

    def isRoot(self):
        return self.parent is None

    def isLeaf(self):
        return len(self.children) == 0

    def root(self):
        if self.isRoot(): return self
        else: return self.parent.root()

    def height(self):
        if self.isRoot(): return 0
        c, h = self, 0
        while c.parent:
            c = c.parent
            h += 1
        return h

    def dfs(self):
        '先深後廣探訪，演算法見[Tree DFS]'
        unvisited = [] # 這是未尋訪堆疊
        cursor = self
        unvisited.extend(cursor.children)
        yield cursor
        while len(unvisited) > 0:
            cursor = unvisited.pop()
            unvisited.extend(cursor.children)
            yield cursor

    def __make_name_table__(self):
        self.name_table = {}
        for c in self.dfs():
            if c.name:
                self.name_table[c.name] = c

    def find_by_name(self, name):
        if not self.name_table:
            self.__make_name_table__()
        return self.name_table[name]

    def __make_address_map__(self):
        '''初始化 address_map'''
        amap = self.__address_map__ = {}
        for c in self.dfs():
            if not amap.has_key(c.type):
                nmap = amap[c.type] = {}
            nmap[c.name] = c

    def address_map(self):
        '''位址由兩部份組成，第一部份是資源名稱，第二部份是類型。

           address_map 是二維字典，
           第一維度為類型，第二維度為資源名稱，
           基本上此字典只存放在根。
        '''
        root = self.root()
        try:
            return root.__address_map__
        except AttributeError:
            root.__make_address_map__()
            return root.__address_map__

    def get(self, type, name):           
        '取出指定位址的文件資源'
        return self.address_map()[type][name]

    def number_children(self):
        cs = self.children
        if self.type in ('book'):
            for i, c in enumerate([c for c in cs if c.type == 'sect1']):
                c.number = i + 1
                c.number_children()
        elif self.type in ('sect1'):
            for i, c in enumerate([c for c in cs if c.type == 'sect2']):
                c.number = i + 1
                c.number_children()
        elif self.type in ('sect2'):
            for i, c in enumerate([c for c in cs if c.type == 'sect3']):
                c.number = i + 1

    def _count_occurence(self, type, o=0):
        for c in self.children:
            if c.type in [type]:
                o = o+1
                c.occurence = o
            o = c._count_occurence(type, o)
        return o

    def count_occurence(self):
        for type in ['code', 'table', 'image', 'define', 'theorem']:
            if self.type in [type]:
                o = o+1
                self.occurence = o
            self._count_occurence(type)

    def dump_type_tree(self, level_limit=10):
        if self.height() < level_limit:
            print '*' * self.height() + self.type
        for c in self.children: c.dump_type_tree(level_limit)

    def print_postfix_tree(self):
        for c in self.children: c.print_postfix_tree()
        print '*' * self.height() + self.type

    def print_tree(self):
        out = '+' * self.height() + self.type + '[' + self.name + ']' \
                     +'#'+str(self.occurence)+'#' + self.section_number() + self.title + '\n'
        for c in self.children: out += c.print_tree()
        return out

    def children_in_type(self, type):
        return [c for c in self.children if c.type == type] 

    def child(self, order=None):
        if not order: return self.children
        else: return self.child()[order]

    def sibling(self, order=None):
        if not order: return self.parent.children
        else: return self.sibling()[order]
    
    def brother(self, order=None):        
        "The sibling shared the same type as this node."
        if not order:
            return self.parent.children_in_type(self.type)
        else:
            return self.brother()[order]

    def order(self):
        "The node's position among brothers."
        if self.isRoot(): return 0
        return self.brother().index(self)

    def path(self):
        '''Nodes from root to self, every tree node has a unique path.
           Path can represente a tree node identically, so we use 
           a path to represent a file in tree file system.
        '''
        if self.isRoot(): return [self]
        path = self.parent.path()
        path.append(self)
        return path

    def order_path(self):
        'a list of order of node in path'
        if self.isRoot(): return [self.order()]
        order_path = self.parent.order_path()
        order_path.append(self.order())
        return order_path
    
    def __getitem__(self, i):                            
        'Get direct child whose index is i.'
        return self.children[i]

    section_number = order_path
    
def Tuple2BTree(root=None, left=None, right=None):
    if root is None:
        raise ValueError, 'Root of binary tree cannot be None!'
    root        = Tree('root', root)

    if left:
        if type(left) is tuple:
            left = Tuple2BTree(*left)
            left.type = 'left'
        else:
            left = Tree('left', left)
        root.append(left)

    if right:
        if type(right) is tuple:
            right = Tuple2BTree(*right)
            right.type = 'right'
        else:
            right = Tree('right', right)
        root.append(right)
    return root

class UnitTest(unittest.TestCase):
    def setUp(self):
        d = Tree('doc')
        d.append(Tree('question', 'q0'))
        d.append(Tree('question', 'q1'))
        d.append(Tree('example', 'e0'))
        d.append(Tree('question', 'q2'))
        d.append(Tree('example', 'e1'))

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
        t.append(Tree('sect', 'section first', name='sect1'))

        self.assertEqual('fhopecc book', t.get('book', 'fhopecc').value)
        self.assertEqual('section first', t.get('sect', 'sect1').value)

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


if __name__ == '__main__':
    unittest.main()
