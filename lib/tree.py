#!/usr/bin/env python
# -*- encoding: utf-8 -*-

"""
A new data structrue: Tree, is created to work like a Python builtins
type, such as list/dict. 

More information can be found at ulfs project:
http://www.yourlfs.org
http://crablfs.sourceforge.net

Copyright (c) 2007-2008 Roc Zhou
"""

__author__ = "Roc Zhou <chowroc.z@gmail.com>"
__date__ = "23 May 2008"
__version__ = "0.3"
__license__ = "GPL v2.0"

import re
from gettext import gettext as _
from copy import copy,deepcopy

import time

try:
	set([])
	# for Python 2.5 and latter
except NameError:
	# for Python 2.3/2.4 compatible
	from sets import Set as set

class RawTree: pass
# Only root['trunk']['branch'] style
#	because Tree may conflicts with many reserved names of Python such as:
#	from, import, as, class, def, return, in, and, or, if, for, while, pass, try, except, finally, ...
# __setitem__ should be overloaded
# __setattr__ should raise TypeError or AttributeError
# Of course, you can still make use of Tree's items functionality:
#	root.trunk['try'] = $value  # ^_^

class TreeExc:
	def __init__(self, message):
		self.message = message

class TreeAttrExc(TreeExc): pass

class TreeItemExc(TreeExc): pass

class TreeTypeExc(TreeExc): pass

class TreePathConvExc(TreeTypeExc): pass

class Tree:
	"""
This class define a builtins like tree type,
thus now we can manipulate a tree very conveniently, like this:
root = Tree(value)
root = Tree(value, data=value1, extra=value2)
root.branch = value
root.branch = Tree(value)
root.branch[key] = value
root.branch[x][y] = value
value = root.branch()
value = root.branch[key]()
setNode(root, ['branch', 'br1'], value)
setNode(root, ('branch', 'br1', (key,), 'br2'), value)
for pathseq, node in root.branch('traverse'): print pathseq, node()
other = Tree(value); root.update(other)
...

Get more details in the methods and operators below.
	"""
	__path_stack = []
	__id_visited = {}
	__used_names = object.__dict__.copy().keys() + [
		'__path_stack', '__id_visited', '__used_names', 
		'__node_value', '__node_items',
		'_Tree__path_stack', '_Tree__id_visited', '_Tree__used_names',
		'_Tree__node_value', '_Tree__node_items',
		'__getattr__', '__setitem__', '__getitem__', '__call__',
		'__add__', '__iadd__', '__cmp__', '__contains__', '__has__',
		'__traverse__', '__update__', '__copy__', '__search__'
	]

	def __init__(self, value, _node_items={}, **kwargs):
		"""
"root = Tree(value)" create a Tree instance as the root node with node 'value'.
A Tree instance can only contain sub Tree instances as 
its attributes or key indexed items, 
as the sub Tree nodes, except its builtins: 
some operators and
internal methods, such as:
%s
these builtins are reserved and raise exceptions if users want to use them,
so these names are special and very rarely used.

The information of sub Tree attributes can be found in __setattr__, and info
about sub Tree key indexed items can be found in __setitem__ and __getitem__.

There are several styles to construct a Tree:
(1) Create a Tree with node value, 
    several sub Tree attributes and key indexed sub Tree items:
root = Tree(value, {'x' : v1, 'y' : v2}, trunk=v3, branch=v4)

(2) The sub Trees, both of attributes and key indexed items, 
    can contain sub Trees, too:
root = Tree(value, {'x' : v1, 'y' : Tree(v2, {'z' : v3}, br=v4)}, trunk=v5, branch=Tree(v6, {'a' : v7}, br=v8))
By this way, a whole more complex Tree can be built all at once, 
of course it can also be built like this
(refer to __setattr__ and __setitem__):

root = Tree(value)
root['x'] = v1
root['y'] = v2
root['y']['z'] = v3
root['y'].br = v4
root.trunk = v5
root.branch = v6
root.branch['a'] = v7
root.branch.br = v8

(3) "root = Tree(Tree(Tree(value)))" has the same effect as "root = Tree(value)"
, it's similar with "dict(dict(dict(zip(seq1, seq2))))"
		""" % ', '.join(self.__used_names)
		# A pseudo private attribute should be assigned by this way:
		self.__dict__['_Tree__node_value'] = None
		self.__dict__['_Tree__node_items'] = {}
		# Must def them before any other operation,
		#	otherwise unexpected exception will be raised, for example:
		#	root = Tree({(('x', 'y'),) : Tree('x/y')})
		#	will get:
		#	{ () :  {(('x', 'y'),): <tree.Tree instance at 0xb7878e0c>} }
		#	because when:
		#	__iadd__() -> getNode()
		#	are called, the segment of code at #2:
		#			try:
		#				next_node = self[key]
		#			except KeyError:
		#				next_node = Tree(None)
		#				self[key] = next_node
		#	will not raise KeyError when "self[key]", because then self.__node_items does not exists.
		#	thus AttributeError will be raised.
		try:
			self.__iadd__(value)
		except TreeTypeExc:
			self.__dict__['_Tree__node_value'] = value
			# __node_value should always be Non-Tree
		try:
			for k, v in _node_items.items(): self.__setitem__(k, v)
		except (AttributeError, TypeError):
			pass
		for k, v in kwargs.items(): self.__setattr__(k, v)

	def __setattr__(self, attr_name, value):
		""""root.branch = value" create a subtree node of 'root' as its
attribute 'branch', and assign the 'value' to 'root.branch' node's private
node value variable: self.__node_value(self._Tree__node_value)

The "root.branch.br1 = value" works on root.branch rather than 
root.branch.br1, thus root must be a Tree instance at first.

If root.branch is the leaf, "root.branch = Tree(value)" have the same effect
as the previous statement, but if 'root.branch' has sub nodes such as
'root.branch.br1', the 'root.branch' and 'root.branch.br1' will been 
replaced by the latter statement.

You can also use "root.branch = Tree(value, br1=value1, br2=value2)" 
to create sub nodes of 'root.branch', refer to __init__, 
and keep the "replacement" problem in head.

Some coding notice: Both "self.attribute = value" and
"setattr(self, attr_name, value)" have been affected by self.__setattr__(), so
"self.__node_value = value" is wrong and will lead to infinite loop, only
self.__dict__ can be use for internal assignment.

Notice that "self.__dict__[attr_name]._Tree__node_value = value" is same as
"self.attribute._Tree__node_value = value", since 'attribute' is also a Tree
instance, this will lead to infinite loop, too.
		"""
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

	def __setitem__(self, key, item):
		"""Sometimes it's valuable to manipulate Tree like this:
root.branch = value
root.branch[1] = value1
root.branch['a'] = value2
For example, you can then build a configuration/options sequence.
		
The solution is to makes 'root.branch.br1[key]' a subtree key indexed by
root.brranch.br1.__node_items[key], it works on root.branch.br1, not
root.branch as __setattr__

Because setattr() has been affected by self.__setattr__, so
"self.__node_items[key]._Tree__node_value = item" is invalid,
only these statements can be used instead:
"self[key].__dict__['_Tree__node_value'] = item" or
"self.__node_items[key].__dict__['_Tree__node_value'] = item"
		"""
		try:
			existed = self.__node_items[key]
			if isinstance(item, Tree):
				subtree = item
				self.__node_items[key] = subtree
				# Replace the key indexed node if the target is a Tree
			else:
				# subtree = self.__node_items[key] is same as:
				subtree = existed
				subtree.__dict__['_Tree__node_value'] = item
		except KeyError:
			if isinstance(item, Tree):
				subtree = item
				self.__node_items[key] = subtree
			else:
				self.__node_items[key] = Tree(item)

	def __getitem__(self, key):
		return self.__node_items[key]

	def __delitem__(self, key):
		del self.__node_items[key]
	
	def __pop__(self, key):
		return self.__node_items.pop[key]

	def __call__(self, action=None, *args, **kwargs):
		"""The most common usage of 'call' is "root.branch()"
which return the node root.branch's value (root.branch._Tree__node_value).
		
The operation of "root.branch.br1()" works on root.branch.br1, not
root.branch, "root.branch.br1()" should return br1's node value rather than
<Tree instance at 0xb7bbb90c> which can be simply returned by 
"itree = root.branch.br1".

It's same for key indexed items: root.branch['x']['y']()

To set the value of the Tree head node is not as easy as sub nodes, 
the latter can make use of "setattr" directly. 
Thus 'set' action is very useful for this.
But it's not allowed to 'set' a node to another Tree, since you can do similar
things by 'update':
root('set', value)

If the value is None, the default behavior is not to change the node value,
because this type of behavior is more useful, especially for ** update **. If
realy want to reset the node value to be None, use the argument 'ignore_none':
root('set', None, ignore_none=False)

Since getNode() call 'set' action, and 'update' also call 'set' through
getNode(), it's possible to pass the 'ignore_none=False' to both of them
making their counterpart nodes change to "emtpy".

"root.branch.br1('items')" return the key indexed subtrees:
root.branch.br1._Tree__node_items

'call' has several other operation that you can see in the method definition,
for example:
"root.branch.br1('traverse', type)" will call "self.__traverse__(type)"
the reason that no root.branch.br1.traverse is used is for less name conflicts
		"""
		if not action:
			return self.__node_value
		elif action == 'set':
			try:
				value = args[0]
				# print value, kwargs
				try:
					ignore_none = kwargs['ignore_none']
				except KeyError:
					ignore_none = True
			except IndexError:
				raise TreeExc, "'set' action is lack of value"
			if isinstance(value, Tree):
				raise TreeTypeExc, "Can't set myself to another: only accept non Tree value, or take 'update' action"
			else:
				if value != None:
					self.__dict__['_Tree__node_value'] = value
				elif not ignore_none:
					self.__dict__['_Tree__node_value'] = None
		elif action == 'items':
			return self.__node_items
		elif action == 'traverse':
			return self.__traverse__()
		elif action == 'update':
			try:
				otree = args[0]
				try:
					ignore_none = kwargs['ignore_none']
				except KeyError:
					ignore_none = True
			except IndexError:
				raise TreeExc, "Lack of other Tree"
			self.__update__(otree, ignore_none)
			# Or make __update__ not allow 'ignore_none', just
			#	__call__('update', ...) accept 'ignore_node' and set the private '__ignore_none'?
		else:
			raise TreeExc, "Invalid action: %s" % action

	def __add__(self, other):
		rtree = copy(self)
		rtree += copy(other)
		return rtree

	def __iadd__(self, other):
		"Update self from another Tree instance"
		self.__update__(other)
		# try:
		# 	self.__update__(other)
		# except TreeTypeExc:
		# 	try:
		# 		# for pathseq, node in other.items():
		# 		# 	setNode(self, pathseq, node())
		# 	except TreeExc, trx:
		# 		# # print "debug: TreeExc"
		# 		# raise TreeTypeExc(trx.msg)
		# 	except (AttributeError, TypeError):
		# 		# # print "debug: AttributeError or TypeError"
		# 		# raise TreeTypeExc(_("Other operand should be a Tree instance or Tree dict"))
		# 		# # TODO: root.trunk += 1 !!!!!!
		return self

 	def __cmp__(self, other):
 		if self is other:
 			return 0
		elif self.__dict__ > other.__dict__:
			# If other is 'int', it has no attribute '__dict__', thus AttributeError will be raised !!!!!!
			return 1
		elif self.__dict__ < other.__dict__:
			return -1
 		elif self() > other():
 			return 1
 		elif self() < other():
 			return -1
		elif self('items') > other('items'):
			return 1
		elif self('items') < other('items'):
			return -1
		# dict comparison will make the subtrees be compared correspondingly.
		else:
			S = set(self.__dict__.keys()) - set(self.__used_names)
			for attr_name in list(S):
				xtree = getattr(self, attr_name)
				ytree = getattr(other, attr_name)
				return cmp(xtree, ytree)
		return 0

	def __traverse__(self):
		"""A generator to yield a path sequences to nodes map, every
item represents a single Tree node. It't better to be called by
__call__('traverse').

Every node of the Tree can be represented by a path sequence such as
a list/tuple, for example:
[] OR () represent the root node,
[['x']] OR (('x',),) --> root['x']
[['x'], 'data'] OR (('x',), 'data') --> root['x'].data
[['x', 'y']] OR (('x', 'y'),) --> root['x']['y']
['trunk'] OR ('trunk',) --> root.trunk
['trunk', ['a']] OR ('trunk', ('a',)) --> root.trunk['a']
['trunk', ['a'], 'data'] OR ('trunk', ('a',), 'data') --> root.trunk['a'].data
['trunk', ['a', 'b']] OR ('trunk', ('a', 'b')) --> root.trunk['a']['b']
['trunk', ['a', 'b'], ['data']] OR ('trunk', ('a', 'b'), 'data') --> root.trunk['a']['b'].data
['branch'] OR ('branch',) --> root.branch
['branch', 'data'] OR ('branch', 'data') --> root.branch.data
So "() : 0" means the 'root' node itself who calls 'traverse'.
	
Thus the key of every item is same as setNode()'s argument pathseq, this
consistency makes compact and concise. See also setNode() function.
		"""
		id_self = id(self)
		if self.__id_visited.has_key(id_self): raise StopIteration
		# To avoid cyclic link problem!
		Tree.__id_visited[id_self] = None
		pathseq = self.__path_stack
		yield pathseq, self
		S = set(self.__dict__.keys()) - set(self.__used_names)
		for attr_name in list(S):
			subtree = getattr(self, attr_name)
			pathseq.append(attr_name)
			for seq, node in subtree.__traverse__(): yield seq, node
			pathseq.pop(-1)
		for key in self.__node_items.keys():
			subtree = self.__node_items[key]
			try:
				if type(pathseq[-1]) is not str:
					last = pathseq[-1]
					last.append(key)
					for seq, node in subtree.__traverse__(): yield seq, node
					last.pop(-1)
				else:
					pathseq.append([key])
					for seq, node in subtree.__traverse__(): yield seq, node
					pathseq.pop(-1)
			except IndexError:
				pathseq.append([key])
				for seq, node in subtree.__traverse__(): yield seq, node
				pathseq.pop(-1)
		if self.__path_stack == []: Tree.__id_visited = {}
		# Clean up __id_visited hash table if traversing is finished.
		# traverse todo:
		#	string to node/value map?
		#	sequence to node/value map?
		#	list of nodes/values?
		#	deep list of nodes?

	# def __update__(self, other, ignore_none=True, validate=False):
	def __update__(self, other, ignore_none=True):
		"""Update self from another Tree instance,
ignore_none=False, see __call__('set', ...)
		"""
		if not isinstance(other, Tree):
			raise TreeTypeExc, "Other operand should be a Tree instance, too"
		# for pathseq, node in other('traverse').items():
		for pathseq, node in other.__traverse__():
			setNode(self, pathseq, node(), ignore_none=ignore_none)
			# setNode(self, pathseq, deepcopy(node()), ignore_none=ignore_none)

	def __str__(self):
		rts = "  ** Tree **\n"
		for pathseq, node in self.__traverse__():
			rts += "%s = %s\n" % (pathseq, repr(node()))
		return rts

def getNode(root, pathseq):
	if not isinstance(root, Tree):
		raise TreeTypeExc, "root node is not a Tree"
	node = root
	for i in range(len(pathseq)):
		if type(pathseq[i]) is str:
			attr_name = pathseq[i]
			if attr_name in Tree._Tree__used_names:
				raise TreeAttrExc, "Attribute name '%s' is reserved and not a Tree node" % attr_name
			try:
				node = getattr(node, attr_name)
			except AttributeError:
				raise AttributeError, "Node %s does not exists" % pathseq[:i+1]
		else:
			for j in range(len(pathseq[i])):
				try:
					key = pathseq[i][j]
					node = node[key]
				except KeyError:
					raise KeyError, "Node %s does not exists" % (pathseq[:i] + [pathseq[i][:j+1]])
	return node

def attrNameCheck(name):
	if not name or re.search('[\W]', name):
		raise TreeExc, "'%s' is not a valid node name of the Tree path name" % name

# _PATH_STACK = []
# def setNode(root, pathseq, value, ignore_none=True, auto=False):
def setNode(root, pathseq, value, ignore_none=True, auto=True):
	"""
Construct a sub Tree node from a path sequence, this path sequence
only represents a single node, for example:
setNode(['branch', 'br1'], value) means root.branch.br1,
setNode(('branch', 'br1', (key,), 'br11'), value) means root.branch.br1[key].br11

Here a nested list/tuple is used to represent a key indexed Tree node,
ranther than "{'br' : key}", because list/dict is unhashable; nor
"('br', key)", because it can not represent the deep key indexed nodes
correctly, such as root['x']['y']['z'], so do the node itself: "('self', 'trunk')"
and "(('self', key), 'trunk')" can only represent its first level sub
nodes.

Thus a Tree node can be represented like this way:
[] OR ()
['branch'] OR ('branch',)
['trunk', 'branch'] OR ('trunk', 'branch')
[['x']] OR (('x',),)
[['x', 'y']] OR (('x', 'y'),)
[['x', 'y'], 'branch'] OR (('x', 'y'), 'branch')
Notice that "(..., ('x',), ('y',), ...)" is same as "(..., ('x', 'y'), ...)",
"(..., ('x', 'y'), ('z',), ..." is same as "(..., ('x', 'y', 'z'), ...)", too

By represented this way, the return value such as __traverse__() can be
used more directly as its argument, thus the operations like __update__()
can be simpler, too. So see also __traverse__()

Notice only the given node will be updated, so in the previous examples,
if root.branch exists, it will not be affected except a sub node been
added to its attributes __dict__, if it does not exist, it will be created
with None as its node value if "auto" argument is True.

If the argument value is a Tree instance, a TreeExc exception will be raised,
as __call__('set', value).

Notice that any type of sequence can be used, so if you pass a string 'branch'
may get a result you don't expect: b.r.a.n.c.h, and 'branch.br1' will cause
TreeExc raises because '.' is not a valid node name.

ignore_none=False, see __call__('set', ...)
	"""
	try:
		next = pathseq[0]
		if type(next) is not str:
		# Attribute name must be a string
		# Otherwise it's a tuple for key indexed sub nodes
			try:
				key = next[0]
				# #2
				try:
					next_node = root[key]
				except KeyError:
					# if auto == False:
	 				#	# raise AttributeError, "Inexistent node %s unless be automatically set to 'None'" % list(pathseq[:i+1])
	 				#	raise AttributeError, "Inexistent node %s unless be automatically set to 'None'" % pathseq
					next_node = Tree(None)
					root[key] = next_node
				next_pathseq = [pathseq[0][1:],] + list(pathseq[1:])
				setNode(next_node, next_pathseq, value, ignore_none)
			except IndexError:
				next_pathseq = pathseq[1:]
				setNode(root, next_pathseq, value, ignore_none)
		else:
			next_name = next
			attrNameCheck(next_name)
			try:
				next_node = getattr(root, next_name)
			except AttributeError:
				# if auto == False:
				#	raise KeyError, "Inexistent node %s unless be automatically set to 'None'" % pathseq
				next_node = Tree(None)
				setattr(root, next_name, next_node)
			next_pathseq = pathseq[1:]
			setNode(next_node, next_pathseq, value, ignore_none)
	except IndexError:
		root('set', value, ignore_none=ignore_none)
	# *
	# except (KeyError, TypeError):
	#	raise TreeTypeExc(_("pathseq must be a sequence, mostly a 2 level nested tuple"))

# def setNode(root, pathseq, value, ignore_none=True, auto=False):
# 	if not isinstance(root, Tree):
# 		raise TreeTypeExc, "root node is not a Tree"
# 	next = root
# 	ilen = len(pathseq)
# 	for i in range(ilen):
# 		if type(pathseq[i]) is str:
# 		# Attribute name must be a string
# 		# Otherwise it's a tuple for key indexed sub nodes
# 			attr_name = pathseq[i]
# 			attrNameCheck(attr_name)
# 			if attr_name in Tree._Tree__used_names:
# 				raise TreeAttrExc, "Attribute name '%s' is reserved and not a Tree node" % attr_name
# 			try:
# 				next = getattr(next, attr_name)
# 			except AttributeError:
# 				if auto == False and i < ilen - 1:
# 					# print "DEBUG", i, pathseq, pathseq[:i+1]
# 					raise AttributeError, "Inexistent node %s unless be automatically set to 'None'" % list(pathseq[:i+1])
# 				else:
# 					setattr(next, attr_name, None)
# 					next = getattr(next, attr_name)
# 		else:
# 			jlen = len(pathseq[i])
# 			for j in range(jlen):
# 				key = pathseq[i][j]
# 				try:
# 					next = next[key]
# 				except KeyError:
# 					if auto == False and (i < ilen - 1 or j < jlen - 1):
# 						raise KeyError, "Inexistent node %s unless be automatically set to 'None'" % (list(pathseq[:i]) + [pathseq[i][:j+1]])
# 					else:
# 						next[key] = None
# 						next = next[key]
# 	next('set', value, ignore_none=ignore_none)
# 	return next

# =============================================================================

	# def __copy__(self, *args): pass

	# def __islike__(self, other):
	# The two trees have the same structure
	# 	pass

	# __has_struct__ ?

	# def __issame__(self, other):
	#	pass

	# def __search__(self, pathseq): pass
		# search todo:
		#	include/exclude
		#	return string or list sequence
		#	copy or assign?
		#	key indexed items?

	# def __contains__(self, other): pass
	# the other equals to one of the subtrees, and 'in' tests

	# def __has__(self, other): pass
	# def __hasthe__(self, other): pass
	# the other is one of the subtrees

# Some problems to resolve:
#	setNode(['trunk', ['x']), value) ?

# AND/OR:
# tree1 & tree2 ?
# tree1 | tree2 ?

# Atomic operation for a Tree node and its subtree?
#	if a Tree node has a attribute: __atomic == True,
#	update can only replace it with a "same" other Node?
# or:
#	def __sync__(self, other):
#	# if self.__is_same__(other), 

def seqconv(pathseq):
	rtstr = ""
	for next in pathseq:
		if type(next) is str:
			attr_name = next
			attrNameCheck(attr_name)
			rtstr += '.' + attr_name
		else:
			try:
				for key in next:
					rtstr += "[%s]" % repr(key)
			except TypeError:
				raise TreePathConvExc(_("Invalid path sequence"))
	return rtstr[1:]

def pathconv(path):
	rtseq = []
	try:
		for next in path.split('.'):
			keys = ''
			try:
				# mo = re.match('(?P<attr_name>[\w]+)(?P<keys>(\[[^\[\]]+\]+)*)', next)
				mo = re.match('^(?P<attr_name>[\w]+)(?P<keys>(\[[^\[\]]+\]+)+)$', next)
				# mo = re.match('(?P<attr_name>[\w]*)(?P<keys>(\[[^\[\]]+\]+)+)', next)
				attr_name = mo.group('attr_name')
				keys = mo.group('keys')
				attrNameCheck(attr_name)
				rtseq.append(attr_name)
				rtseq.append([s.strip("\'\"") for s in re.findall('\[([^\[\]]+)\]', keys)])
			except AttributeError:
				if next.startswith("["):
					mo = re.match("^(?P<keys>(\[[^\[\]]+\]+)+)$", next)
					keys = mo.group('keys')
					rtseq.append([s.strip("\'\"") for s in re.findall('\[([^\[\]]+)\]', keys)])
				else:
					attr_name = next
					attrNameCheck(attr_name)
					rtseq.append(attr_name)
	except AttributeError:
		raise TreePathConvExc(_("Invalid path string"))
	return rtseq

### class FastTree(Tree): pass

### class ImmutableTree(Tree): pass

import unittest
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
        for i in root('traverse'):
            self.assertEqual('value', i)

if __name__ == '__main__':
    unittest.main()
