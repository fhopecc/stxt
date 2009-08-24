#!/usr/bin/python
# -*- encoding: utf-8 -*-

# Author: 周鹏(Chowroc)
# Date: 2006-06-10

# 2006-06-06: 确定配置文件基本格式，并实行 get 解析
# 2006-06-07: 以递归实现变量替换
# 2007-02-02: 调整

"""该程序操作一个以平面方式表示的树形配置文件，取得或设置相关配置。
有效选项校验应该属于应用程序自己的事情，
ctree 只负责读取配置，并做一些基本的解析

* 配置文件格式如下(可参考 sysctl 的 sysctl.conf 或 postfix 的 main.cf)：
fs_backup.format = "bid, btime, archive, type, list, host";
fs_backup.default = "full"; # comment
# more comment
# 即使用 = 分隔键、值对，使用 "" 包含值，使用 ; 分隔记录
# 使用 # 注释，空行不受影响

* 选项的值亦支持多行模式：
test.long.value = "The value of the options
also support
multilines mode";

* 使用变量：
fs_backup.datadir = "/var/task/fs_backup";
fs_backup.tagfile = "$datadir/.tag";
# 或使用绝对路径：
# fs_backup.tagfile = "${fs_backup.datadir}/.tag";
# 即绝对路径要使用 {}

* 跨文件读取变量(未实现)，以单独的函数或方法实现较好，也许以函数形式比较好，因为可供外部调用

* 通配符使用 及正则表达式(未实现)，注意变量不能支持正则

* 加入属性支持(未实现)

* 一选项多值支持，使用参数指定(未实现)

* 加入 python 数据结构及命令支持(未实现)

* 加入 shell 命令支持(未实现)

* 加入时间策略支持，如 date 命令的 date -d '1 days ago' +%Y%m%d 的效果(未实现)

* 提供应用程序使用 -o $option 时可用的解析函数(未实现)

* 支持从标准输入读入(未实现)

* 支持不带""号的情况(未实现)

* 支持交互模式(未实现)

* 支持 list 命令(相当于 ls)及其递归模式(未实现)

* 支持两种模式下(交互与非交互)对某一选项的设置操作(未实现)

* 支持 file faker(未实现)？

* !!! 可以传递 list，也可以传递 dict，最好可以传递一个 Class Config 作为参数？"""

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

class CTreeEx:
	def __init__(self, errno=1, strerr='CTree,Exception'):
		self.errno = int(errno)
		self.strerr = str(strerr)
	# def logError(self):
	def logError(self, logerr=outlog):
		print >> sys.stderr, self.strerr
		if self.errno == 1:  logerr.error(self.strerr)
		elif self.errno == 127:  logerr.critical(self.strerr)

class CTree:
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
