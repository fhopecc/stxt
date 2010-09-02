# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from google.appengine.api import users
from model import *

class UnitTest(unittest.TestCase):
    def setUp(self):
        owner = users.User("test@gmail.com")
        self.h = Homestay(name=u"測試民宿", 
                          owner=owner)
        self.h.put()
        Room(homestay=self.h, name=u"雙人房").put()

        Room(homestay=self.h, name=u"四人房").put()
        
    def testRoomSet(self):
        rooms = self.h.room_set

        self.assertEqual(2, rooms.count())

    def testAvailableRoom(self):
        self.assertEqual(u"測試民宿", self.h.name)

    def tearDown(self):
        hs = Homestay.all().filter("name", u"測試民宿")
        for h in hs:
            for r in h.room_set:
                r.delete()
            h.delete()
            
if __name__ == '__main__':
    unittest.main()
    '''tests = unittest.TestSuite()
    tests.addTest(UnitTest("testREFERENCE"))
    tests.addTest(UnitTest("testCBLOCK"))
    tests.addTest(UnitTest("testPARA"))
    tests.addTest(UnitTest("testReference"))
    tests.addTest(UnitTest("testNEWLINE"))
    #tests.debug()
    #runner = unittest.TextTestRunner()
    #runner.run(tests)'''
