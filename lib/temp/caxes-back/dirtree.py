#!/usr/bin/python
# -*- encoding: utf-8 -*-

def path_parse(seperator='/', prefix='/'):
	pathseq = ()
	return pathseq

class DirTree:
	def __init__(self, hashtree={}, pathseq):
		self.hashtree = {}
		self.seperator = seperator
		self.prefix = prefix
		self._pathseq = []
		self._next_node = hashtree

	def clear(self):
		self.hashtree.clear()

	def __getitem__(self, key):
		key = normpath(key).lstrip(self.prefix)
		_pathseq = key.split(self.seperator)[1:]
		_next_node = self.hashtree
		for name in _pathseq:
			try:
				_next_node = _next_node[name]
			except KeyError:
				raise KeyError, "Invalid path '%s'" % key
			except TypeError:
				return _next_node
				# raise TypeError, "Invalid path '%s'" % key
		return _next_node

	def __setitem__(self, key, value):
		key = os.path.normpath(key)
		pathseq = key.split("/")[1:]
		_next_node = self.hashtree
		# _prev_node = _next_node
		# print "TYPE: %d" % self.type
		for name in pathseq[:-1]:
			# print "*** name: %s ***" % name
			try:
				temp = _next_node
				_next_node = _next_node[name]
				_prev_node = temp
				_prev_name = name
				# print "prev: %s, prev_name: %s, next: %s" % (_prev_node, _prev_name, _next_node)
			except KeyError:
				# print "KeyError, prev: %s, next: %s" % (_prev_node, _next_node)
				temp = _next_node
				_next_node[name] = 1
				_next_node = _next_node[name]
				_prev_node = temp
				_prev_name = name
			except TypeError:
				# Reach the current leaf node
				# print "TypeError, prev: %s, prev_name: %s, next: %s" % (_prev_node, _prev_name, _next_node)
				if _prev_node[_prev_name] == DIR:
					_prev_node[_prev_name] = {name : 1}
					_next_node = 1  # Same as:
					# _next_node = _prev_node[_prev_name]
					_prev_node = _prev_node[_prev_name]
					_prev_name = name
				else: # _prev_node[_prev_name] == FILE:
					raise TypeError, "Invalid file path '%s'" % key
		try:
			name = pathseq[-1]
		except IndexError:
			raise KeyError, "An empty path"
		# print "LEAF, prev: %s, prev_name: %s, next: %s" % (_prev_node, _prev_name, _next_node)
		if _prev_node[_prev_name] == DIR:
			_prev_node[_prev_name] = {name : self.type}
		elif type(_prev_node[_prev_name]) is dict:
			try:
				_leaf_node = _next_node[name]
				if _leaf_node == DIR or type(_leaf_node) is dict:
					if self.type == DIR:
						pass
					else:
						raise TypeError, "Assign dir '%s' to file is not permitted" % key
				else: # _leaf_node == FILE:
					if self.type == FILE:
						pass
					else:
						raise TypeError, "Assign file '%s' to dir is not permitted" % key
			except KeyError:
				_next_node[name] = self.type
		else:
			raise TypeError, "Invalid file path '%s'" % key
	def __contains__(self, key):
		try:
			value = self[key]
			# print "key: %s, value: %s" % (key, value)
			if value == self.type:
				pass
			elif self.type == DIR and type(value) is dict:
				pass
			else:
				return False
		except KeyError:
			return False
		included = False
		for tin, xexs in self.tx_dict.items():
			if key.startswith("%s/" % tin) or key == tin:
				included = True
				for xex in xexs:
					if key.startswith("%s/" % xex) or key == xex: included = False
				if included: return True
		return included
	def has_key(self, key):
		return self.__contains__(key)
	def __iter__(self):
		self.pathseq.append('')
		temp = self._next_node
		try:
			for name, value in self._next_node.iteritems():
				self.pathseq[-1] = name
				path = "/" + "/".join(self.pathseq)
				if path in self: yield path
				self._next_node = value
				for path in iter(self): yield path
		except AttributeError:
			pass
			# if self.type == FILE: pass
			# else: raise
		self._next_node = temp
		self.pathseq.pop(-1)
		# if not self.pathseq: raise StopIteration
	def pop(self, key, default=None):
		key = os.path.normpath(key)
		# pathseq = key.split("/")[1:-1]
		# name = pathseq[-1]
		# dirname = "/" + "/".join(pathseq)
		i = key.rfind("/")
		dirname = key[:i]
		name = key[i+1:]
		_prev_node = self[dirname]
		try:
			value = _prev_node.pop(name)
			return value
		except KeyError:
			if default != None:
				return default
			else:
				raise KeyError, "Invalid path '%s'" % key
	def keys(self):
		rtv = []
		for path in iter(self): rtv.append(path)
		return rtv
	def update(self, other):
		for key in other.keys(): self[key] = None

class FsSnapTree(DirTree):
	pass

class HugedbDirTree(DirTree):
	pass
