#!/usr/bin/env python
# -*- encoding: utf-8 -*-

"""
Import a Python module from a file pathname string, rather than simply
"import" since "import" depend too much on PYTHONPATH or sys.path env,
which may cause confusing of name space, or not so convenient.

For example:
	mod = use_module("/usr/local/share/saunit/check-points/netstat.py")
	mod.run()
OR:
	mod = use_module("/usr/local/share/saunit/check-points/telnet")
Both style are right.
"""

__author__ = "Roc Zhou <chowroc.z@gmail.com>"
__date__ = "11 June 2008"
__version__ = "0.3.1"
__license__ = "GPL v2.0"

import os,sys

class ModuleUtilError(Exception): pass

class UseModuleError(ModuleUtilError): pass

class NotPythonModule(ModuleUtilError): pass

def use_module(fname):
	"""
	Insert the path fname into sys.path when trying to import the module,
	and pop out it from sys.path after imported, so at last the sys.path
	should not be changed.

	>>> from caxes import utils
	>>> m = utils.use_module("/tmp/1")
	>>> m.__file__
	'/tmp/1.py'	
	"""
	inserted = False
	try:
		try:
			path = os.path.dirname(fname)
			if not os.path.isdir(path):
				raise UseModuleError, (1, "Path '%s' does not exist or is not a directory." % path)
			mod_name = os.path.basename(fname)
			if mod_name.endswith(".py"):
				if not os.path.isfile(fname):
					raise UseModuleError, (2, "Module '%s' does not exist or is not a regular file." % mod_name)
				mod_name = mod_name[:-3]
			else:
				if not os.path.isfile("%s.py" % fname):
					if os.path.isfile(fname):
						raise NotPythonModule, (1, "%s is not a Python module" % fname)
					else:
						raise UseModuleError, (2, "Module '%s' does not exist or is not a regular file." % mod_name)
			# sys.path.insert(0, path)
			# inserted = True
			# # module = __import__(os.path.join(path, mod_name))
			# try:
			#	mod_temp = sys.modules.pop(mod_name)
			#	# There may be a sys module has the same name that have been imported,
			#	#	then __import__() will do nothing, thus pop it first to release the influence.
			#	module = __import__(mod_name)
			#	sys.modules[mod_name] = mod_temp
			# except KeyError:
			#	module = __import__(mod_name)
			# # sys.path.pop(0)
			import imp
			module = imp.load_module(mod_name, *imp.find_module(mod_name, [path]))
			return module
		# except ImportError, exc:
		#	raise UseModuleError, (3, str(exc))
		except SyntaxError, exc:
			raise NotPythonModule, (2, "%s is not a Python module, %s" % (fname, exc))
			# Maybe TODO:
			# raise UseModuleError, (2, "%s is not a Python module, %s" % (fname, exc))
	finally:
		# if inserted and sys.path[0] == path:
		#	# The imported module may have changed the sys.path
		#	sys.path.pop(0)
		pass

def module_cwd(mod_name):
	"""
	Get the absolute current working directory(cwd) of the module,
	rather than the script's cwd, which can be get from sys.argv[0]:
	os.path.dirname(os.path.abspath(sys.argv[0]))
	"""
	fname = sys.modules[mod_name].__file__
	cwd = os.path.dirname(os.path.abspath(fname))
	return cwd

def import_file(mod_name, fname):
	if fname.startswith("/"):
		real = fname
	else:
		mcwd = module_cwd(mod_name)
		real = os.path.realpath(os.path.join(mcwd, fname))
	return use_module(real)
