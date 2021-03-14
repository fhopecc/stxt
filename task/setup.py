'''
環境設定
'''
import sys, os, shutil
from pathlib import Path

disk = Path(r'C:\\')
r = Path(os.path.dirname(__file__))

def setup_vim():
    '設定 Vim 環境'
    _vimrc = r / '..' / 'config' / 'vim' / '_vimrc'
    vimdir = next(disk.glob('*x86*\\*[V]im*'))
    cmd = f'copy "{_vimrc}" "{vimdir}"'
    print(cmd)
    os.system(cmd)

def set_sysenv(var, val):
    cmd = 'setx %s "%s" /M >nul 2>nul' % (var, val)
    if not os.system(cmd) == 0:
        raise 'set_sysenv("%s", "%s") failed!' % (var, val)

def add_path(p):
    ps = os.environ['PATH'].split(';')
    if p not in ps:
        ps.append(p)
        #import pdb;pdb.set_trace()
        ps = list(set(ps))
        set_sysenv('PATH', ';'.join(ps))
        #print 'Append "%s" to PATH.' % p

#add_path(os.path.dirname(sys.argv[0]))

#add_path(r'C:\Program Files\GnuWin32\bin')


'''
0.1:將路徑加入程式搜尋路徑 environ['PATH']
'''
if __name__ == "__main__":
#    from optparse import OptionParser
#    usage = u"usage: %prog log [options]"
#    parser = OptionParser(usage, version="%prog 0.1", 
#             description=u"環境設定自動化工具組"
#        )
#    parser.add_option("-p", "--path", dest="path", 
#                       help=u"將路徑加入程式搜尋路徑 environ['PATH']")
#
#    (options, args) = parser.parse_args()
#
#    if options.path: add_path(options.path)
    setup_vim()
