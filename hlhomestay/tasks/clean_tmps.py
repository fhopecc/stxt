import fnmatch
import os

def clear_tmps(dir):
  for f in os.listdir(dir):
    if os.path.isdir(f):
      clear_tmps(f)
    elif fnmatch.fnmatch(f, '*~'):
      path = os.path.join(dir, f)
      print 'remove ' + path
      os.remove(path)

clear_tmps(os.path.join(os.path.dirname(__file__), '..'))
