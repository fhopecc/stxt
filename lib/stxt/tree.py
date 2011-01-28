# coding=utf8
from __future__ import with_statement
import sys, logging
from os import path
from tree import *
import log

logger = logging.getLogger('stxt.tree')

NAMED_NODE = ['sect1', 'sect2', 'sect3', 'sect4', 'sect5',
              'define', 'theorem', 'question']

class Node(object):
    def __init__(self, type='', value='', title='', name='', 
                 source='__string__', spos=None, epos=None, 
                 slineno=None, lineno=None, 
                 elineno=None, scol=None, ecol=None):
        self.type, self.value = type, value
        self.title = title
        self.name = name
        self.parent, self.children = None, []
        self.number = None       # It's section number
        self.occurence = None    # It's table number
        self.name_table = None    
        self.name_table = {}    
        self.source = source

        # slineno should be decrepated
        self.lineno = lineno or slineno
        self.slineno = lineno or slineno
        self.spos = spos

    def __str__(self):
        m = "%s:\n[\n%s\n]" % (self.type, self.value)
        for c in self.children:
            m+=str(c)
        return m

    def __repr__(self):
        return str(self)

    @property
    def kids(self):
        return self.children

    def append(self, *nodes):
        for n in nodes: 
            n.parent = self
            self.children.append(n)
        return self

    def replace_children(self, children):
        self.children = []
        if type(children) is list:
            self.append(*children)
        else:
            self.append(children)

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

    def dump(self, level_limit=10):
        if self.height() < level_limit:
            name = ''
            if self.name:
                name = self.name
            print '%s%s[%s]' % ('*' * self.height(), self.type, name)
        for c in self.children: c.dump(level_limit)

    def __make_name_table__(self):
        self.name_table = {}
        for c in self.dfs():
            if c.name:
                self.name_table[c.name] = c

    def find_by_name(self, name):
        if not self.name_table:
            self.__make_name_table__()
        return self.name_table[name]

    def make_name_table(self):
        for c in self.dfs():
            key = c.name
            if not key: key = c.title
            #if not key: key = id(c)
            if key:
                if self.name_table.has_key(key):
                    msg = "duplicating key %s at %s:%s" % (key,
                              self.source, self.slineno)
                    logger.warn(msg)
                else:
                    self.name_table[key] = c

    def dump_name_table(self):
        for k in self.name_table.keys():
            print '%s' % k

    def getrefnode(self):
        u'取出參考節點'
        if type <> 'reference':
            raise TypeError, "[%s] isn't [reference]." % type

    def get(self, key):
        u'取出指定名稱之文件資源'
        if self.isRoot(): return self.name_table[key]
        else: 
          r = self.root()
          return r.get(key)

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

    def dump(self, level_limit=10):
        if self.height() < level_limit:
            name = ''
            if self.name:
                name = self.name
            print '%s%s[%s]%s:%s' % ('*' * self.height(), self.type,
                name, self.source, self.slineno)
        for c in self.children: c.dump(level_limit)

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

class TitledNode(Node):
    def __init__(self, title):
        Node.__init__(self)

class ReferenceNode(Node):
    def __init__(self, address='', label=''):
        Node.__init__(self, 'reference')
        self.address = address
        self.label= label

    def reftree(self): 
        ref = self.get(self.address)
        return ref
    
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

# This is for back capability, Tree should be deprecated.
Tree = Node
