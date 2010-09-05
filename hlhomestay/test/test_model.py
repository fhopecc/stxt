# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from google.appengine.api import users
from model import *
from datetime import date
from datetime import timedelta
import datetime
# before must be zero
today = date.today()
yestorday = date.today() - timedelta(days=1)
tomorrow = date.today() + timedelta(days=1)
nextweek = date.today() + timedelta(days=7)
next2weeks = date.today() + timedelta(days=14)

class UnitTest(unittest.TestCase):
    def setUp(self):
        from datetime import date
        owner = users.User("test@gmail.com")
        self.h = Homestay(name=u"測試民宿", 
                          owner=owner)
        self.h.put()

        self.room1 = Room(homestay=self.h, name=u"雙人房")
        self.room1.put()
        
        Reservation(room=self.room1, name=u"張簡稜剛", checkin=today, 
                    checkout=tomorrow).put()

        Reservation(room=self.room1, checkin=nextweek, 
                    checkout=nextweek + timedelta(days=1)).put()

        self.room2 = Room(homestay=self.h, name=u"四人房")
        self.room2.put()

        Reservation(room=self.room2, name=u"沈懿嬅", checkin=next2weeks, 
                    checkout=next2weeks + timedelta(days=3)).put()
        
    def testRoomSet(self):
        rooms = self.h.room_set

        self.assertEqual(2, rooms.count())

    def testReservation(self):
        self.assertEqual(2, self.room1.reservation_set.count())
        r = self.room1.reservation_set.get()

        self.assertEqual(u"雙人房", r.room.name)

    def testRoomStatus(self):
        rs = self.h.rooms_status(yestorday)
        self.assertEqual(0, sum(1 for r in rs))

        rs = self.h.rooms_status(today)
        self.assertEqual(2, sum(1 for r in rs))

        rs = self.h.rooms_status(today)
        r = rs.next()
        self.assertEqual('Reservation', r.kind()) 
        self.assertEqual(u'張簡稜剛', r.name) 
        self.assertEqual(u"雙人房", r.room.name)

        r = rs.next()
        self.assertEqual('Room', r.kind()) 
        self.assertEqual(u"四人房", r.name)

        rs = self.h.rooms_status(tomorrow)
        self.assertEqual(2, sum(1 for r in rs))

        rs = self.h.rooms_status(tomorrow)
        r = rs.next()
        self.assertEqual('Room', r.kind()) 
        self.assertEqual(u"雙人房", r.name)
        r = rs.next()
        self.assertEqual('Room', r.kind()) 
        self.assertEqual(u"四人房", r.name)

        rs = self.h.rooms_status(next2weeks + timedelta(days=2))
        self.assertEqual(2, sum(1 for r in rs))

        rs = self.h.rooms_status(next2weeks + timedelta(days=2))
        r = rs.next()
        self.assertEqual('Room', r.kind()) 
        self.assertEqual(u"雙人房", r.name)

        r = rs.next()
        self.assertEqual('Reservation', r.kind()) 
        self.assertEqual(u'沈懿嬅', r.name) 
        self.assertEqual(u"四人房", r.room.name)

        rs = self.h.rooms_status(next2weeks + timedelta(days=3))
        r = rs.next()
        self.assertEqual('Room', r.kind()) 
        self.assertEqual(u"雙人房", r.name)

        r = rs.next()
        self.assertEqual('Room', r.kind()) 
        self.assertEqual(u"四人房", r.name)

    def testAvailableRooms(self):
        rs = self.h.available_rooms(yestorday)
        self.assertEqual(0, sum(1 for r in rs))

        rs = self.h.available_rooms(tomorrow)
        self.assertEqual(2, sum(1 for r in rs))

        res = self.room1.reservation_set.filter('checkin <=', today)
        self.assertEqual(1, res.count())
        res = [ r for r in res if today < r.checkout]
        self.assertEqual(1, len(res))

        rs = self.h.available_rooms(today)
        self.assertEqual(1, sum(1 for r in rs))
        rs = self.h.available_rooms(today)
        r = rs.next()
        self.assertEqual(u"四人房", r.name)

    def tearDown(self):
        hs = Homestay.all().filter("name", u"測試民宿")
        for h in hs:
            for r in h.room_set:
                for res in r.reservation_set:
                    res.delete()
                r.delete()
            h.delete()
            
if __name__ == '__main__':
    unittest.main()
    #tests = unittest.TestSuite()
    #tests.addTest(UnitTest("testAvailableRooms"))
    #tests.addTest(UnitTest("testCBLOCK"))
    #tests.addTest(UnitTest("testPARA"))
    #tests.addTest(UnitTest("testReference"))
    #tests.addTest(UnitTest("testNEWLINE"))
    #tests.debug()
    #runner = unittest.TextTestRunner()
    #runner.run(tests)'''
