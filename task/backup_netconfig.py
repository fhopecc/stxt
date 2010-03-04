# coding=utf8
import sys, os
from telnetlib import Telnet
cmd = 'execute backup allconfig FG100A 192.168.1.101'
t = Telnet('192.168.1.254')
t.read_until('login:')
t.write('admin\n')
t.read_until('Password:')
t.write('!tsinim9\n')
t.read_until('#')
t.write('%s\n' % cmd)
t.read_until('done.')
