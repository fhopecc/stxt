# coding=utf8
from __future__ import with_statement
from os import path
from wget import wget
from logging import config
from zipfile import ZipFile
import sys, os, shutil, logging , socket

config.fileConfig(r'task\log.conf')
logger = logging.getLogger()

def update_hlland(docdate, docno, password):
    '更新地政觸控系統'
    url = 'http://att.hl.gov.tw/SENDATT_FILE/%s/376550400A_%s.zip' % \
          (docdate, docno)
    file = os.path.join('tmp', '376550400A_%s.zip' % docno)
    '''wget(url, file)'''
    unzip(file, password)

def unzip(file, password):
    zip = ZipFile(file)
    rar = zip.namelist()[0]
    logger.info('unzip %s to %s' % (file, rar))
    #zip.extractall('tmp')
    cmd = "{0} e -o+ -p{1} {2} {3}".format(r'lib\bin\rar', 
          password, rar, os.path.join('tmp', 'hlland'))
    logger.info(cmd)
    os.system(cmd)

def isalive(host):
    targetIP = socket.gethostbyname(host)  
    socket.setdefaulttimeout(1)
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)  
  
    result = s.connect_ex((targetIP, 445))  
    if result == 0 :  
        return True
    s.close() 
    return False

def deploy():
    targets  = ['96tt003', '96tt006', '97tt024', 
                '97tt025', '97tt027', '97tt040']
    for t in targets:
        if isalive(t):
            logger.info('{0}:445 is OPENED.'.format(t))
            cmd = 'net use T: /delete'
            os.system(cmd)
            cmd = r'net use T: \\{0}\HL'.format(t)
            os.system(cmd)
            dir = os.path.join('tmp', 'hlland')
            for root, dirs, files in os.walk(dir):
                for f in files:
                    src  = os.path.join(root, f)
                    try:
                        shutil.copy(src, 'T:')
                    except IOError:
                        pass


        else:
            logger.info('{0}:445 is CLOSED.'.format(t))

def usage():
    usage =  update_hlland.__doc__ + '\n'
    usage += '用法: %s docdate docno password\n'
    usage += 'docdate: 文件日期\n'
    usage += 'docno: 文號\n'
    usage += 'password: 壓縮檔密碼\n'
    return usage % os.path.basename(sys.argv[0]) 

if __name__ == '__main__':
    try:
        if len(sys.argv) < 4: raise IndexError()
#update_hlland(*sys.argv[1:])
        deploy()
    except IndexError:
        logger.info(usage()) 
#print usage()
"""
task :expend_patchr => [libdir, :unzip_patchz] do
		m = `#{rar} e -o+ -p#{password} #{patchr} #{libdir}`
		logging "expend #{patchr} "


require 'net/ftp'
require 'net/http'
require 'uri'
require 'ping'
docdate  = '0990108' #來文日期
docno    = '0990000318' #文號
password = '632891'   #密碼

tmpdir   = 'tmp/hlland'
libdir   = "#{tmpdir}/lib"
patchdir = "#{tmpdir}/patch"
patchz   = "#{patchdir}/#{docno}.zip" 
patchr   = "#{tmpdir}/patch.rar"
unzip    = 'lib/bin/unzip'
rar      = 'lib/bin/rar'
targets  = ['96tt003', '96tt006', '97tt024', '97tt025', '97tt027', '97tt040']
def logger
	@logger ||= Logger.new("log/#{File.basename(__FILE__)}.log")
end
def logging m
  logger.info m 
	puts m
end
def copy_task 
  libdir = 'tmp/hlland/lib'
	Dir.glob("#{libdir}/*").each do |f|
		unless File.basename(f) == 'v_qry1.exe'
			FileUtils.copy f, "T:/"
		end
	end
end
def copy_to target
	if Ping.pingecho target, 10, 445
	  system 'net use T: /delete'
	  system 'net use T: \\\\' + target + '\\HL'
    copy_task
    logging "copy to #{target} completely!"
	else
    logger.error "#{target} is dead!"
	end
end
directory patchdir
directory libdir
namespace 'hlland' do
	task :unzip_patchz => :download_patchz do
		m = `#{unzip} -o  #{patchz} -d #{tmpdir}`
		m  =~ /tmp.*\.rar/
		tf =  $&
    FileUtils.mv tf, patchr
		logging "unzip #{patchz} to #{patchr}"
	end
	task :expend_patchr => [libdir, :unzip_patchz] do
		m = `#{rar} e -o+ -p#{password} #{patchr} #{libdir}`
		logging "expend #{patchr} "
	end
	task :clear do
	  FileUtils.rm patchz
	  FileUtils.rm patchr
		Dir.glob("#{libdir}/*").each do |f|
			FileUtils.rm f
		end
	end
  desc "update hlland query program"
	task :update => :expend_patchr do
		copy_to '96tt003'
		copy_to '96tt006'
		copy_to '97tt024'
		copy_to '97tt025'
		copy_to '97tt027'
		copy_to '97tt040'
	  #Rake::Task["hlland:clear"].invoke
  end
end
"""
