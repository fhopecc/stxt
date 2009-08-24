#!/usr/bin/python
# -*- encoding: utf-8 -*-

# Author: 周鹏(Chowroc)
# Date: 2006-06-10

"""This program manipulate plain presented tree text files, read and write
some relative configurations.

The validation checking is the responsibility of the program that call it,
ftree only read the file and do some basic parsing.

@ The format of the configuration files should be(the sysctl.conf and main.cf
of postfix can be referred):
fs_backup.format = "bid, btime, archive, type, list, host";
fs_backup.default = "full"; # comment
# more comment
# So uses = as seperator of key and value, "" includes the value, 
# 	and ; seperates the records
# Use # as comments, and ignore the blank lines

@ The multilines mode is supported for value:
test.long.value = "The value of the options
also support
multilines mode";

@ Make use of variables:
fs_backup.datadir = "/var/task/fs_backup";
fs_backup.tagfile = "$datadir/.tag";
# Absolute path:
# fs_backup.tagfile = "${fs_backup.datadir}/.tag";
# So absolute path must use {}

*** TODO List ***
@ Cross file variable reading
@ Wildcard and regular expression, but no re for variables
@ Add attribute support
@ One option, several values, to appoint that by parameters
@ Add python structure and command support
@ Add shell command support
@ Add time strategy support, such as date -d '1 days ago' +%Y%m%d
@ Supply -o $option support for the program that call it
@ Add stdin reading
@ Support value without ""(for numeric)
@ Support interactive mode
@ Support list(like ls) and its recursive mode
@ Add support for setting operation of a option(interactive and noninteractive)
*** TODO List ***

@ Pass a list/dict as method argument, or a class "Tree", and return a dict or
"Tree"
"""

import re
import os
import getopt
import sys

import logger
from pathtools import *

program = os.path.basename(sys.argv[0])
resplit = re.compile(r';\s*\n|;\s*(?!\\)#*.*\n|^\s*$|^\s*#.*$', re.M)
### separator = r';\s*\n|;\s*(?!\\)#*.*\n|^[\s\t]*$|^\s*#.*$'
# ^\s*$, empty line; ^\s*(?\\)#.*$, comment line
# rematch = re.compile(r'^\s*(.+)\s*(?!\\)=\s*(?!\\)"(.+)(?!\\)"\s*')

def main():
	try:
		opts, args = getopt.getopt(sys.argv[1:], 'gsp:k:vh', 
			['config=', 'get', 'set', 'prefix=', 'key=', 'only-value', 'help'])

		confile = args[0]
		action = ''
		optmap = {}
		prefix = ''
		showopt = True
		for o, v in opts:
			if o in ['-g', '--get']:
				action = 'get'
			if o in ['-s', '--set']:
				action = 'set'
			if o in ['-p', '--prefix']:
				prefix = v
			if o in ['-k', '--key']:
				optmap[v] = ''
			if o in ['-v', '--only-value']:
				showopt = False
			if o in ['-h', '--help']:
				print usage
				sys.exit(0)

		# ctobj = CTree(confile, optmap, prefix)
		ctobj = CTree()
		if action == 'get':
			optmap = ctobj.get(confile, prefix, optmap)
		elif action == 'set':  pass
		else:
			print 'Empty or wrong action(get/set)'
			print usage
			sys.exit(0)

		if prefix:  prefix += '.'
		for key in optmap.keys():
			if showopt:  print '%s%s = "%s"' % (prefix, key, optmap[key])
			else:  print '%s' % optmap[key]

	except getopt.GetoptError, goEx:
		strerr = "getopt error: %s, %s" % (goEx.opt, goEx.msg)
		sys.stderr.write('%s\n' % strerr)
		# outlog.error(strerr)
		sys.exit(1)

	except IndexError:
		strerr = "Lack of config file name"
		print strerr
		# outlog.info(strerr)
		print usage
		sys.exit(0)

	except CTreeEx, ctEx:
		ctEx.logError(logerr=outlog)

if __name__ == '__main__':
	outlog = logger.get(program)
	usage = """program usage: %s [OPTIONS] config-file
	OPTIONS:
	-g|--get, 取得配置选项
	-s|--set, 设置配置选项
	-p|--prefix, 配置选项键前缀(默认为空)
	-k|--key, 键，'prefix.key' 即为实际的选项键
	-v|--only-value, 只打印选项值
	-h|--help""" % program

	main()
	# try:
	#	main()
	# except CTreeEx, ctEx:
	#	ctEx.logError(logerr=outlog)
	# except Exception, Ex:  print Ex.__str__; outlog.error(Ex.__str__)
	# catch all
else:
	outlog = logger.get(__name__)
	usage = """module usage: %s""" % __name__

class FTreeEx:
	def __init__(self, errno=1, strerr='CTree,Exception'):
		self.errno = int(errno)
		self.strerr = str(strerr)
	# def logError(self):
	def logError(self, logerr=outlog):
		print >> sys.stderr, self.strerr
		if self.errno == 1:  logerr.error(self.strerr)
		elif self.errno == 127:  logerr.critical(self.strerr)

class FTree:
	def __init__(self):
		# self.confile = confile
		self.confile = ''
		self.prefix = ''
		self.optmap = {}

	def get(self, confile, prefix, optmap={}, flags=[]):
		# if type(confile) is file:
		#	self.confile = confile
		# else:
		#	self.confile = open(confile)
		self.confile = confile
		if prefix:
			self.prefix = prefix
		if optmap:
			if type(optmap) is list:
				optmap = dict(map(None, optmap, []))
			self.optmap = optmap
		self.prefix = normalize(self.prefix)
		if self.prefix:  self.prefix += '.'
		optemp = {}
		keymap = {}

		try:
			# self.config = resplit.split(self.confile.read())
			self.config = resplit.split(open(confile).read())
		except IOError, strerr:
			strerr = 'IOError: %s' % strerr
			raise CTreeEx(strerr=strerr)
		except TypeError, strerr:
			strerr = 'TypeError: %s' % strerr
			raise CTreeEx(strerr=strerr)

		try:
			for line in self.config:
				### print 'DEBUG: BOL, %s, EOL' % line
				# 此处 line 指一个 key/value 对，实际可能是多行(多行模式)
				## mo = rematch.match(line, re.M|re.S)
				mo = re.match(r'^\s*(.+)\b\s*(?!\\)=\s*(?!\\)"(.+)(?!\\)"\s*', line, re.M|re.S)
				if not mo:  continue
				option = mo.group(1)
				keys = self.optmap.keys()
				# if option in [ self.prefix + key for key in self.optionmap.keys() ]:
				value = mo.group(2)
				for key in keys:
					### print '%s, %s' % (key, option)
					if 'r' in flags or 'regexp' in flags:
					# 模式匹配
						komo = re.match(self.prefix + key, option)
						if komo: keys.append(basename(option))
					elif option == self.prefix + key:
					# 找到匹配
						self.optmap[key] = value
					else:  continue
					# 未找到匹配
					mo = re.match(r'\${\b(.+)\b}|\$(\w+)', value)
					if mo:
					# 存在变量
						if mo.group(1):  tk = mo.group(1)
						elif mo.group(2):  tk = self.prefix + mo.group(2)
						optemp[tk] = ''
						keymap[key] = tk
					break
			if optemp:
				### self.get(optemp, self.prefix, flags) ??????
				### cttemp = CTree(self.confile, optemp); cttemp.get()
				# *** file.tell() or make use of lines(self.config)? ***
				cttemp = CTree()
				optemp = cttemp.get(self.confile, self.prefix, optemp)
				# 递归调用以解释变量
				# 但为什么直接调用自身不行？
				for key in keymap.keys():
					tk = keymap[key]
					if not optemp[tk]:
						strerr = 'No variable option ${%s} found in the configuration.' % tk
						raise CTreeEx(strerr=strerr)
					self.optmap[key] = re.sub(r'\${\b.+\b}|\$\w+', optemp[tk], self.optmap[key])

			return optmap
		except TypeError, strerr:
			raise CTreeEx(strerr=strerr)
