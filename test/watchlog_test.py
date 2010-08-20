# coding=utf8
from __future__ import with_statement
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from task.watchlog import *

case = 'Aug 19 02:45:01 192.168.1.254 local0.notice date=2010-08-19 time=02:45:50 device_id=FG100A2106400343 log_id=0107042103 type=event subtype=pattern pri=notice vd=root status=update virdb=yes idsdb=yes libav=yes aven=yes imap=yes smtp=yes pop3=yes http=yes ftp=yes fcni=yes fdni=yes idsmn=yes idssn=yes rbldb=yes msg="Fortigate updated virdb(12.258) idsdb(2.850) aven(up-to-date) idsen(up-to-date) rbldb(up-to-date)"Aug 19 09:51:11 192.168.1.254 local0.info date=2010-08-19 time=09:52:00 device_id=FG100A2106400343 log_id=0104032006 type=event subtype=admin pri=information vd=root user=admin ui=GUI(192.168.1.101) action=login status=success reason=none msg="User admin login successfully from GUI(192.168.1.101)"Aug 19 10:07:11 192.168.1.254 local0.info date=2010-08-19 time=10:07:59 device_id=FG100A2106400343 log_id=0104032007 type=event subtype=admin pri=information vd=root user=admin ui=GUI(192.168.1.101) action=logout status=success reason=timeout msg="GUI session timeout from GUI(192.168.1.101)"Aug 19 10:07:11 192.168.1.254 local0.info date=2010-08-19 time=10:07:59 device_id=FG100A2106400343 log_id=0104032007 type=event subtype=admin pri=information vd=root user=admin ui=GUI(192.168.1.101) action=logout status=success reason=timeout msg="GUI session timeout from GUI(192.168.1.101)"'

class UnitTest(unittest.TestCase):
    def testParseLog(self):
        logs = parselog(case)
        log = logs.next()
        self.assertEqual("Fortigate updated virdb(12.258) idsdb(2.850) " 
                         "aven(up-to-date) idsen(up-to-date) "
                         "rbldb(up-to-date)", log.message) 
        self.assertEqual("Aug 19 02:45:01", log.timestamp)
        self.assertEqual("192.168.1.254", log.ip)

        log = logs.next()
        self.assertEqual("User admin login successfully from "
                         "GUI(192.168.1.101)", log.message)

if __name__ == '__main__':
    unittest.main()
    #tests = unittest.TestSuite()
    # TABLE parsing will failed in yacc debug mode    
    #tests.addTest(UnitTest("testTable"))
    #runner = unittest.TextTestRunner()
    #runner.run(tests)
    #tests.debug()
