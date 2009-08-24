#!/usr/bin/python
# -*- encoding: utf-8 -*-

# Author: ÖÜÅô
# Date: 2006-02-03
# Email: chowroc.z@gmail.com

import os
import sys
import shutil

def search(filename, inew):
	try:
		if type(inew) is not str:
			strerr = "%s is not a valid LIST item string" % inew
			print >> sys.stderr, strerr
			raise "search list error"
		file = open(filename, 'r')
		for item in file.readlines():
			item = item.strip()
			if inew == item: return
		raise "search list error"
	except Exception, (errno, errstr):
		strerr = "search list faild: %d, %s" % (errno, errstr)
		print >> sys.stderr, strerr
		raise "search list error"

def insert(filename, inew):
	"Èç¹ûÒ»¸öÌõÄ¿²»ÔÚÁĞ±íÎÄ¼şÖĞ£¬ÔòÖÃÈë"
	try:
		if type(inew) is not str:
			strerr = "%s is not a valid LIST item string" % inew
			sys.stderr.write("%s\n" % strerr)
			return
		open(filename, 'a')  # touch $filename
		if os.path.isfile(filename): mode = 'r+'
		# else: mode = 'w'
		file = open(filename, mode)
		for item in file.readlines():
			item = item.strip()
			if inew == item: return
			# Èç¹ûÒÑ¾­´æÔÚÔòÊ²Ã´Ò²²»×ö
		file.write("%s\n" % inew)
		file.close()
	except Exception, (errno, errstr):
		strerr = "insert into list faild: %d, %s" % (errno, errstr)
		sys.stderr.write("%s\n" % strerr)

def delete(filename, inew):
	"´ÓÁĞ±íÎÄ¼şÖĞÉ¾³ıÒ»¸öÌõÄ¿"
	try:
		if type(inew) is not str:
			strerr = "%s is not a valid LIST item string" % inew
			sys.stderr.write("%s\n" % strerr)
			raise "delete from list error"
		file = open(filename, 'r+')
		items = file.readlines()
		i = -1
		for item in items:
			itmp = item.strip()
			if inew == itmp:
				i = items.index(item)
				items.pop(i)
				break
		if i < 0:
			strerr = "LIST item: %s was not found" % inew
			sys.stderr.write("%s\n" % strerr)
			raise "delete from list error"
		else:
			file.seek(0)
			file.truncate()
			file.writelines(items)
		file.close()
	except Exception, (errno, errstr):
		strerr = "pull list faild: %d, %s" % (errno, errstr)
		sys.stderr.write("%s\n" % strerr)
		raise "delete from list error"

# *** Most of list items will be files && directories path names ***

### def issub(fn, dn):

def find_top_files(tmpl):
	import glob
	tmpl.sort()
	topfiles = []
	invalids = []
	file1 = ''
	for item in tmpl:
		item = item.strip()
		if not os.path.exists(item) and len(glob.glob(item)) == 1:
		# There may be 'dir/*' style string
			strerr = "[31m%s is not a valid file or directory[00m" % item
			print >> sys.stderr, strerr
			invalids.append(item)
			continue
		if not file1:
			if item == '': continue #
			file1 = item
			topfiles.append(file1)
		elif item.startswith(file1):
			# 'file1' is the parent directory of 'item'
			continue
		elif file1.startswith(item):
			file1 = item
			topfiles[-1] = file1
		else:
			file1 = item
			topfiles.append(file1)
	return topfiles, invalids

def rmtree(pathname):
	if os.path.isdir(pathname):
		shutil.rmtree(pathname)
	else:
		os.remove(pathname)

def copytree(pathname):
	if os.path.isdir(pathname):
		shutil.copytree(pathname)
	else:
		shutil.copy(pathname)
