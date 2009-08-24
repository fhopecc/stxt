#!/usr/bin/env python
# -*- encoding: utf-8 -*-

__author__ = "Roc Zhou <chowroc.z@gmail.com>"
__date__ = "11 June 2008"
__version__ = "0.3"
__license__ = "GPL v2.0"

import sys
import types

import utils
import tree
from tree import *

# Future TODO:
#	For configuration links!!!
#
 
class OptionsExc(Exception): pass

class Options(Tree):
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

 	def __call__(self, action=None, *args, **kwargs):
 		# if not action:
		if action == None:
 			if type(self._Tree__node_value) is types.LambdaType:
 				try:
 					return self._Tree__node_value()
 				except TypeError, exc:
 					raise OptionsExc, exc.message
			else:
				return self._Tree__node_value
		else:
	 		Tree.__call__(self, action, *args, **kwargs)

	def __setattr__(self, attr_name, value):
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
				subtree = existed
				subtree.__dict__['_Tree__node_value'] = value
				# Only replace the node value
		except AttributeError:
		# if self.attribute does not exists, assign it a EMPTY node
			if isinstance(value, Tree):
				subtree = value
				self.__dict__[attr_name] = subtree
			else:
				self.__dict__[attr_name] = Options(value)

 	def __setitem__(self, key, item):
		try:
			# existed = self.__node_items[key]
			existed = self._Tree__node_items[key]
			if isinstance(item, Tree):
				subtree = item
				# self.__node_items[key] = subtree
				self._Tree__node_items[key] = subtree
				# Replace the key indexed node if the target is a Tree
			else:
				# subtree = self.__node_items[key] is same as:
				subtree = existed
				subtree.__dict__['_Tree__node_value'] = item
		except KeyError:
			if isinstance(item, Tree):
				subtree = item
				# self.__node_items[key] = subtree
				self._Tree__node_items[key] = subtree
			else:
				# self.__node_items[key] = Tree(item)
				self._Tree__node_items[key] = Options(item)

class ConfigExc:
	def __init__(self, message):
		self.message = message

class TreeConfig:
	def __init__(self, obj, name=None):
	# TODO:
	# 1. I found it's also necessary to load config from a module, not only its name!
	# 2. Maybe you want to merge two TreeConfig to one!
	# 3. Can pass a Tree instance directly as a configuration
		self.fname = None
		self.name = None
		self.cfmod = None
		self.modify = {}
		if isinstance(obj, Tree):
			self.root = Options(obj)
			return
		self.fname = fname = obj
		self.name = name
		try:
			# TODO: make it possible to pass in a module directly
			# if type(fname) is types.ModuleType:
			#	self.cfmod = fname
			self.cfmod = utils.use_module(fname)
			try:
				self.root = getattr(self.cfmod, name)
			except TypeError:
				raise ConfigExc, "Option name in the module config file should be specified"
			# TODO: make the module also be legal.
			if not isinstance(self.root, Tree):
				raise TypeError, "Not a valid Tree config: %s" % (fname, name)
		except utils.UseModuleError, exc:
			raise ConfigExc, "No such config module: %s" % fname
		except AttributeError, exc:
			# raise ConfigExc, "Option name in the module config file should be specified"
			raise ConfigExc, "Can't find the specified option '%s' in the module config file" % name

	def read(self, pathname, default=None):
	# def read(self, pathname, default=None, exc_handle=True):
	# def read(self, pathname, default=None, strict=True):
	# TODO:
	# If strict, raise exception if no default given?
		try:
			if type(pathname) is str:
				node = tree.getNode(self.root, tree.pathconv(pathname))
			else:
				pathseq = pathname
				pathname = tree.seqconv(pathseq)
				node = tree.getNode(self.root, pathseq)
			# if not isinstance(node, Tree):
			#	raise ConfigExc, "Not a valid Tree option: %s" % (pathname)
			# TODO:
			# Add forced comment...
			return node()
		except (AttributeError, KeyError), exc:
			return default
		# Future TODO:
		# except OptionExc:
		#	return deffault

	# TODO: readNode() be similar to tree.getNode() but can pass pathname string
	# def readNode(pathname, default=None):

	def set(self, pathname, value):
		# try:
		if type(pathname) is str:
			tree.setNode(self.root, tree.pathconv(pathname), value, ignore_none=False, auto=True)
		else:
			pathseq = pathname
			pathname = tree.seqconv(pathseq)
			tree.setNode(self.root, pathseq, value, ignore_none=False, auto=True)

	# def flush
			
