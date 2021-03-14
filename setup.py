# set_env.py
# automatic set environment
import sys
import os

def add_os_path(path):
    cmd = 'setx path "%%path%%;%s"' % path 
    if not os.system(cmd) == 0:
        print('%s failed!' % cmd)
    print("add %s into os executing path." % cmd) 
# add python install path into OS path
python_dir = os.path.dirname(sys.executable) 
add_os_path(os.path.join(python_dir, 'Scripts'))
