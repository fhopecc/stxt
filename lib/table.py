#!/usr/bin/env python
# -*- encoding: utf-8 -*-

"""
A new data structrue: Table, is created to work like a Python builtins
type, such as list/dict, and as Tree
"""

# Reference: http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/496770

__author__ = "Roc Zhou <chowroc.z@gmail.com>"
__date__ = "3 June 2008"
__version__ = "0.3"
__license__ = "GPL v2.0"

import types

def listHasDup(L):
	visited = {}
	for x in L:
		if visited.has_key(x):
			return True
		else:
			visited[x] = None
	return False

class TableExc:
	def __init__(self, message):
		self.message = message

class Table:
	# TODO:
	# default value?
	def __init__(self, fields=[], keys=[], rowtype="list"):
	# What about:
	# def __init__(self, *args, **kwargs):
		if not fields:
			raise TableExc, "Empty fields Table are meaningless"
		if listHasDup(fields):
			raise TableExc, "Table initializing fails because of duplicated fields"
		if listHasDup(keys):
			raise TableExc, "Table initializing fails because of duplicated keys"
		# TODO: Is it necessary to forbid string as fields? If is, write unit tests:
		if type(fields) is types.StringType:
			raise TableExc, "Table fields will be characters which may not be your expect"
		column_indexes = [ fields.index(x) for x in fields ]
		self.fields = dict(zip(fields, column_indexes))
		self.columns = fields
		self.indexPool = {}
		for k in keys:
			if k not in fields:
				raise TableExc, "Unkown key '%s' for initializing" % k
			self.indexPool[k] = {}
			# Why not a tuple map:
			# ('a', 1), ('c', 3), ('b', 2), ('a', 4)
			# -- when duplicated ?
		# self.keys = keys
		self.serial = 0
		self.records = {}
		# Use incremental serial number as its key!
		self.sn = 0
		self.__selected = self.records
		self.__select_level = 0
		self.__rowtype = rowtype
		
		# ****** Have problems !!!??? *******
		# self.__row_proc = self.__row_pass
		# if rowtype == "hash":
		#	self.__row_proc == self.__row_hash
		# ***********************************

	def __len__(self):
		return len(self.records)

	def __row_pass(self, row):
		return row

	def __row_hash(self, row):
		return dict(zip(self.columns, row))

	def __row_proc(self, row):
		if self.__rowtype == "hash":
			return dict(zip(self.columns,row))
		else:
			return row

	# def set_rowtype(rowtype):

	# def sort(self, key):

	# TODO:
	# add cond support to only appoint several fields rather than all,
	#	and make others to be None automatically if the user specified.
	# def insert(self, record=[], cond={}):
	#	"""
	#	If record is passed, cond will be ignored.
	#	"""
	def insert(self, record=[]):
		"""
		Can be considered as a type of counterpart of SQL INSERT,
		record is a sequence, so string may also work but not your expect.
		"""
		if record:
			if len(record) != len(self.fields):
				raise ValueError, "Not a valid record: %s, lack of value for some fields." % record
			# self.records.append(list(record))
			self.records[self.serial] = list(record)
			self.serial += 1
			# Does list convert a good idea ?
			for key in self.indexPool.keys():
				col = self.fields[key]
				value = record[col]
				# Setup the index for this value:
				try:
					self.indexPool[key][value].append(self.serial - 1)
				except KeyError:
					self.indexPool[key][value] = [self.serial - 1]

	def __iadd__(self, other):
		"""
		Compare with List:
		>>> L = ["a", "b", "c"]
		>>> L += "d"
		>>> print L
		['a', 'b', 'c', 'd']		
		"""
		self.insert(other)
		return self
			
	# TODO:
	# 1. More complicated conditions.
	# 2. Use stack to achieve this?
	def iselect(self, cond={}):
		"""
		Can be considered as a type of counterpart of SQL SELECT,
		this is the iterator method.
		"""
		try:
			if self.__select_level == 0:
				keys = cond.keys()
				for key in keys:
					if key not in self.indexPool:
						raise KeyError, "No key '%s' found to select, you can traverse all records by iterator or just pass {} as conditions" % key
			key = cond.keys()[0]
			value = cond[key]
			cond.pop(key)
			# col = self.fields[key]
			selected = {}
			# Find out the index of the value first:
			for idx in self.indexPool[key][value]:
				if idx in self.__selected:
				# same as:
				# if self.__selected.has_key(idx):
					selected[idx] = self.records[idx]
					# At first __selected points to all records.
			self.__selected = selected
			self.__select_level += 1
			for sn, rec in self.iselect(cond):
				yield sn, rec
			self.__select_level -= 1
			if self.__select_level == 0:
				self.__selected = self.records
		except IndexError:
			for sn, row in self.__selected.iteritems():
				yield sn, self.__row_proc(row)
	
	def select(self, cond={}):
		rows = {}
		for sn, row in self.iselect(cond): rows[sn] = row
		return rows

	def __iter__(self):  return self

	def next(self):
		"""
		The iterator
tableobj = Table(["name", "time", "type", "status", "message"], ["name", "time"])
import time
from datetime import datetime
tableobj += ["telnet", datetime(*time.localtime()[:6]), "ALRM", "PASS", ""]
tableobj += ["netstat", datetime(*time.localtime()[:6]), "ALRM", "FAIL", "TOO MANY SYNC"]
tableobj += ["telnet", datetime(*time.localtime(time.time() - 3600)[:6]), "ALRM", "FAIL", "TELNET 0.0.0.0:80 FAILS"]
tableobj += ["vmstat", datetime(*time.localtime()[:6]), "STAT", "FIELDS", ", ".join(["SWPD", "FREE", "BUFF", "CACHE"])]
tableobj += ["vmstat", datetime(*time.localtime()[:6]), "STAT", "RECORD", ", ".join(["%s" % x for x in [160, 238712, 294328, 1293752]])]
for sn, row in tableobj: print sn, row		
		"""
		if self.sn == self.serial:
			self.sn = 0
			raise StopIteration
		else:
			self.sn += 1
			try:
				return self.sn - 1, self.__row_proc(self.records[self.sn - 1])
			except KeyError:
				# May be deleted
				pass

	def __str__(self):
		rts = "  ** Table **\n"
		for sn, row in self:
			rts += "<%d> %s\n" % (sn, row)
		return rts

	# def index(self, 
