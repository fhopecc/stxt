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

    def height(self):
        if self.isRoot(): return 0
        c, h = self, 0
        while c.parent:
            c = c.parent
            h += 1
        return h

    def _dfs(self, unvisited):
        for c in self.children:
            c._dfs(unvisited)
        unvisited.append(self)

    def dfs(self):
        '''depth-first search
               It calls _dfs() to construct a list of nodes in dfs order,
               then it enumerates the list to implement the generator.
           TODO: 
               Modify it not constructing node first, just find the next
               node on demand.
        '''
        unvisited = []
        self._dfs(unvisited)
        for n in unvisited:
            yield n

    def __make_name_table__(self):
        self.name_table = {}
        for c in self.dfs():
            if c.name:
                self.name_table[c.name] = c

    def find_by_name(self, name):
        if not self.name_table:
            self.__make_name_table__()
        return self.name_table[name]

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

    def section_number(self, level=0):
        '''Return a list of section numbers by level.
        '''
        if self.number is None: return '' 
        else: 
            if self.parent.number is None: return [self.number]
            elif level == 0: return [self.number]
            else: 
                ns = self.parent.section_number(level-1)
                ns.append(self.number)
                return ns

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

    def print_type_tree(self, level_limit=10):
        if self.height() < level_limit:
            print '*' * self.height() + self.type
        for c in self.children: c.print_type_tree(level_limit)

    def print_postfix_tree(self):
        for c in self.children: c.print_postfix_tree()
        print '*' * self.height() + self.type

    def print_tree(self):
        out = '+' * self.height() + self.type + '[' + self.name + ']' \
                     +'#'+str(self.occurence)+'#' + self.section_number() + self.title + '\n'
        for c in self.children: out += c.print_tree()
        return out

    def order(self):
        "order is defined as the node's position among sibling"
        return self.sibling().index(self)
        
    def sibling(self, order=None):
        if not order:
            return self.parent.children
        else:
            return self.parent.children[order]
       
class UnitTest(unittest.TestCase):
    def testSibling(self):
        r = DocTreeNode('book', 'book value', name='book')
        r.append(DocTreeNode('sect1', 'child1 value', name='child1'))
        r.append(DocTreeNode('sect1', 'child2 value', name='child2'))

        self.assertEqual(0, r.find_by_name('child1').order())

        self.assertEqual('child2 value', 
                r.find_by_name('child1').sibling(1).value)

    def testNameTable(self):
        r = DocTreeNode('book', 'book value', name='book')
        r.append(DocTreeNode('child', 'child value', name='child'))
        self.assertEqual('book value', r.find_by_name('book').value)
        self.assertEqual('child value', r.find_by_name('child').value)

    def testPrevious(self):
        r = DocTreeNode('book', 'book value', name='book')
        r.append(DocTreeNode('sect1', 'sect1 value', name='child1'))
        r.append(DocTreeNode('sect2', 'sect2 value', name='child2'))
        r.append(DocTreeNode('sect1', 'sect1 value', name='child3'))
        sect1s = [c for c in r.children if c.type == 'sect1']
        child3 = r.find_by_name('child3')
        self.assertEqual(1, sect1s.index(child3))

DocTreeNode = Tree

if __name__ == '__main__':
    unittest.main()
