# coding=utf8
from __future__ import with_statement
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from task.watchlog import *

case = 'Aug 19 02:45:01 192.168.1.254 local0.notice date=2010-08-19 time=02:45:50 device_id=FG100A2106400343 log_id=0107042103 type=event subtype=pattern pri=notice vd=root status=update virdb=yes idsdb=yes libav=yes aven=yes imap=yes smtp=yes pop3=yes http=yes ftp=yes fcni=yes fdni=yes idsmn=yes idssn=yes rbldb=yes msg="Fortigate updated virdb(12.258) idsdb(2.850) aven(up-to-date) idsen(up-to-date) rbldb(up-to-date)"Aug 19 09:51:11 192.168.1.254 local0.info date=2010-08-19 time=09:52:00 device_id=FG100A2106400343 log_id=0104032006 type=event subtype=admin pri=information vd=root user=admin ui=GUI(192.168.1.101) action=login status=success reason=none msg="User admin login successfully from GUI(192.168.1.101)"Aug 19 10:07:11 192.168.1.254 local0.info date=2010-08-19 time=10:07:59 device_id=FG100A2106400343 log_id=0104032007 type=event subtype=admin pri=information vd=root user=admin ui=GUI(192.168.1.101) action=logout status=success reason=timeout msg="GUI session timeout from GUI(192.168.1.101)"Aug 19 10:07:11 192.168.1.254 local0.info date=2010-08-19 time=10:07:59 device_id=FG100A2106400343 log_id=0104032007 type=event subtype=admin pri=information vd=root user=admin ui=GUI(192.168.1.101) action=logout status=success reason=timeout msg="GUI session timeout from GUI(192.168.1.101)"'

class UnitTest(unittest.TestCase):
    def testParse3CLog(self):
        logs = read3clogs(case)
        i = 0
        for log in logs:
            print log[0]
            i += 1;
        self.assertEqual(4, i)
       
    def testParseLog(self):
        logs = parselog(case)
        log = logs.next()
        self.assertEqual("Fortigate updated virdb(12.258) idsdb(2.850) " 
                         "aven(up-to-date) idsen(up-to-date) "
                         "rbldb(up-to-date)", log.message) 
        self.assertEqual("Aug 19 02:45:01", log.timestamp)
        self.assertEqual("192.168.1.254", log.hostname)

        log = logs.next()
        self.assertEqual("User admin login successfully from "
                         "GUI(192.168.1.101)", log.message)

    def testEnterasysParse(self):
        case = '''Aug 20 06:43:42 10.66.4.254 local4.debug  APR 23 20:31:47 0.0.0.0-1 SIM[152242736]: dot1s_sm.c(1033) 7205 %% Discarding BPDU:Cannot process BPDU on port(7) until migration timer expires
Aug 20 06:43:42 10.66.4.254 local4.debug  APR 23 20:31:47 0.0.0.0-1 SIM[152242736]: dot1s_sm.c(1033) 7206 %% Discarding BPDU:Cannot process BPDU on port(8) until migration timer expires
Aug 20 06:43:42 10.66.4.254 local4.debug  APR 23 20:31:47 0.0.0.0-1 SIM[152242736]: dot1s_sm.c(1033) 7207 %% Discarding BPDU:Cannot process BPDU on port(7) until migration timer expires
Aug 20 06:43:42 10.66.4.254 local4.debug  APR 23 20:31:47 0.0.0.0-1 SIM[152242736]: dot1s_sm.c(1033) 7208 %% Discarding BPDU:Cannot process BPDU on port(8) until migration timer expires
Aug 20 06:43:43 10.66.4.254 local4.debug  APR 23 20:31:48 0.0.0.0-1 SIM[152242736]: dot1s_sm.c(1033) 7209 %% Discarding BPDU:Cannot process BPDU on port(4) until migration timer expires
Aug 20 06:43:43 10.66.4.254 local4.debug  APR 23 20:31:48 0.0.0.0-1 SIM[152242736]: dot1s_sm.c(1033) 7210 %% Discarding BPDU:Cannot process BPDU on port(3) until migration timer expires
Aug 20 06:48:16 10.66.4.254 local4.debug  APR 23 20:36:22 0.0.0.0-1 UNITMGR[188324872]: unitmgr.c(3492) 7211 %% Configuration propagation successfulAug 20 08:18:36 10.66.4.254 local4.crit  APR 23 22:06:42 0.0.0.0-1 USER_MGR[1]: 7212 %% User:NONE logged out from 10.66.4.56(telnet)'''

        logs = read3clogs(case)
        i = 0
        for log in logs:
            print log[0]
            i += 1;
        self.assertEqual(8, i)

        logs = parse_enterasys_log(case)
        log = logs.next()
        self.assertEqual("Discarding BPDU:Cannot process BPDU on port(7) "+
                         "until migration timer expires", log.message)
        self.assertEqual("Aug 20 06:43:42", log.timestamp)
        self.assertEqual("10.66.4.254", log.hostname)
        self.assertEqual("local4", log.facility)
        self.assertEqual("debug", log.servity)

        log = logs.next()
        self.assertEqual("User:admin logged out from 10.66.4.56(telnet)", 
                         log.message)
        self.assertEqual("Aug 19 02:45:01", log.timestamp)
        self.assertEqual("192.168.1.254", log.hostname)
        self.assertEqual("local4", log.facility)
        self.assertEqual("crit", log.servity)

if __name__ == '__main__':
    unittest.main()
    #tests = unittest.TestSuite()
    # TABLE parsing will failed in yacc debug mode    
    #tests.addTest(UnitTest("testTable"))
    #runner = unittest.TextTestRunner()
    #runner.run(tests)
    #tests.debug()
