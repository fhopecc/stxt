'''
環境設定
安裝使用套件
至 jupyter lab 的命令提示字元查看執行情形
 下載編好的wheel網址
https://www.lfd.uci.edu/~gohlke/pythonlibs/#gdal
'''
import sys, os, subprocess
from pathlib import Path
from shutil import copyfile
version = 0.1
disk = Path(r'C:\\')
r = Path(os.path.dirname(__file__))
configdir = r / '..' / 'config'
bindir = r / '..' / 'bin'

def custom_vim():
    '設定 Vim 環境'
    _vimrc = r / '..' / 'config' / 'vim' / '_vimrc'
    vimdir = next(disk.glob('*x86*\\*[V]im*'))
    cmd = f'copy "{_vimrc}" "{vimdir}"'
    print(cmd)
    os.system(cmd)

def custom_rime():
    '''自訂小狼毫輸入法環境'''
    p = Path(r'C:\Program Files (x86)\Rime\weasel-0.14.3')
    fdir = configdir / 'rime' 
    fs = ['symbols.yaml', 'weasel.custom.yaml', 'default.yaml']
    print('開始設定小狼毫輸入法環境……')
    for f in fs:
        cp(fdir / f, p / 'data')

def backup_rime():
    '''備份小狼毫輸入法設定檔'''
    p = Path(r'C:\Program Files (x86)\Rime\weasel-0.14.3')
    fs = ['default.yaml']
    fdir = configdir / 'rime' 
    print('備份小狼毫輸入法設定檔……')
    for f in fs:
        cp(p / 'data' / f, fdir)

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

def install(package):
    '安裝套件'
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

def upgrade(package):
    '升級套件'
    subprocess.check_call([sys.executable, "-m", "pip", "install", "--upgrade", package])

def setup_package():
    '設定套件'
    upgrade('pip')
    install('statsmodels')
    install('lxml')
    install('seaborn')
    install('pandas_ods_reader')
    install('xlrd')
    install('openpyxl')
    install('pandas')
    install('pyarrow')
    install('backupy')
    install('scipy')
    install('bs4')
    install('pymongo')
    install('invoke')
    install('google-api-python-client')
    install('oauth2client')
    install('httplib2')
    install('jupyterlab')
    install('easyblogger')
    setup_geopandas()
    setup_levenshtein()

def setup_gdal():
    'http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86_64.exe'
    fname =  'GDAL-3.2.2-cp39-cp39-win_amd64.whl'
    gdal = bindir / fname
    if gdal.exists():
        install(str(gdal))
    else:
       raise Exception(f'需先下載{fname}至本機')

def setup_levenshtein():
    fname = 'c:\Python39\wheel\python_Levenshtein-0.12.2-cp39-cp39-win_amd64.whl'
    install(fname)

def setup_fiona():
    setup_gdal()

    fname =  'Fiona-1.8.18-cp39-cp39-win_amd64.whl'
    fiona = bindir / fname
    if fiona.exists():
        install(str(fiona))
    else:
       raise Exception(f'需先下載{fname}至本機')
 
def setup_geopandas():
    setup_fiona()
    install('geopandas')

def cp(f, t):
    '''更新檔案f至目錄t。且異動原檔前，先備份舊檔，再覆蓋原檔'''
    d = t / f.name
    b = t / f'{f.stem}_bk{f.suffix}'
    if d.exists():
        copyfile(d, b)
    copyfile(f, d)
    print(f'替換自訂{f.name}完成！')

def setup():
    print('開始自訂環境……')
    custom_rime()

if __name__ == "__main__":
    setup_package()
