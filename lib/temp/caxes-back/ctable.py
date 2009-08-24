#!/usr/bin/python
# -*- encoding: utf-8 -*-

import re

class table: pass

class CTable:
	def __init__(self, tfile='', fields=[]):
		self.tfile = open(tfile, 'r+')
		self.fields = fields
	def select(self, match={}):
	# Maybe it's better to make match a SQL like string
	## to support '>', '<' and date comparision
	## I think there should be 4 data types for table: string, integer, date, bool
		records = []
		for line in [ S.strip() for S in self.tfile.readlines() ]:
			record1 = re.split(',\s*', line)
			record1 = dict(zip(self.fields, record1))
			found = 1
			for k, v in match.items():
				if record1[k] != v: found = 0; break
			if found:
				records.append(record1)
		self.tfile.seek(0)
		return records
	def append(self, record1):
		self.tfile.seek(0, 2)
		self.tfile.write("%s\n" % ', '.join(record1))
		self.tfile.seek(0)

# ctb = CTable('/var/fs_backup/.table', ['identity', 'type', 'time', 'archive'])
# records = ctb.select(match={'identity' : 'myopt', 'type' : 'full'})
# print records
