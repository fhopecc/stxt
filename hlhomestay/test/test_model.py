# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from google.appengine.api import users
from model import *
from datetime import date
from datetime import timedelta
from datetime_iterator import datetimeIterator
import datetime
# before must be zero
today = date.today()
yestorday = date.today() - timedelta(days=1)
tomorrow = date.today() + timedelta(days=1)
nextweek = date.today() + timedelta(days=7)
next2weeks = date.today() + timedelta(days=14)

class UnitTest(unittest.TestCase):
    def setUp(self):
        SysHoliday(name=u"稜剛生日",
                   date=date(2010, 7, 29)).put()

        SysHoliday(name=u"聖誕節",
                   date=date(2010, 12, 25)).put()

        owner = users.User("test@gmail.com")
        self.h = Homestay(name=u"測試民宿", 
                          owner=owner)
        self.h.put()

        # Negative holiday list
        Holiday(homestay= self.h,
                name=u"聖誔節不漲價", 
                date=date(2010, 12, 25), 
                isholiday=False).put()
        # Positive holiday list
        Holiday(homestay= self.h,
                name=u"品果生日", 
                date=date(2010, 7, 23), 
                isholiday=True).put()

        self.h2 = Homestay(name=u"測試民宿2", 
                          owner=owner)
        self.h2.put()


        self.room1 = Room(homestay=self.h, name=u"雙人房")
        self.room1.put()

        self.res1 = Reservation(room=self.room1, name=u"張簡稜剛",
                    checkin=yestorday - timedelta(days=1), 
                    checkout=yestorday)
        self.res1.put()


        Reservation(room=self.room1, name=u"張簡稜剛", checkin=today, 
                    checkout=tomorrow).put()

        Reservation(room=self.room1, checkin=nextweek, 
                    checkout=nextweek + timedelta(days=1)).put()

        self.room2 = Room(homestay=self.h, 
                          name=u"四人房", 
                          price=2000,
                          holiday_price=4000
                         )
        self.room2.put()


        self.res2 = Reservation(room=self.room2, 
                                name=u"沈懿嬅", 
                                checkin=next2weeks, 
                                checkout=next2weeks + timedelta(days=3))
        self.res2.put()

        self.res3 = Reservation(room=self.room2, 
                                name=u"沈懿嬅", 
                                checkin=date(2010, 7, 28), 
                                checkout=date(2010, 8, 4))

        self.res3.put()

        self.room3 = Room(homestay=self.h2, name=u"五人房")
        self.room3.put()

        Reservation(room=self.room3, name=u"張簡嘉品", checkin=next2weeks, 
                    checkout=next2weeks + timedelta(days=3)).put()

    def tearDown(self):
        for h in Homestay.all():
            for r in h.room_set:
                for res in r.reservation_set:
                    res.delete()
                r.delete()
            h.delete()

    def testURLMaker(self):
        self.assertEqual('/admin/%s' % self.res1.key(), 
                         self.res1.admin_show_path())
        
    def testRoomSet(self):
        rooms = self.h.room_set
        self.assertEqual(2, rooms.count())


    def testReservation(self):
        self.assertEqual(3, self.room1.reservation_set.count())
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

    def testMonthlyRoomStatus(self):
        mrs = self.h.monthly_rooms_status(today.year,
                                         today.month)
        self.assertEqual(5, len(mrs))
        for w in mrs:
            for d in w:
                if d['date'] == today:
                    for r in d['rooms_status']:
                        if r.kind() == 'Reservation':
                            self.assertEqual(u'張簡稜剛', r.name)
                elif d['date'] == next2weeks:
                    for r in d['rooms_status']:
                        if r.kind() == 'Reservation':
                            self.assertEqual(u'沈懿嬅', r.name)
                        elif r.kind() == 'Room':
                            self.assertEqual(u'雙人房', r.name)

    def testRecentlyReservations(self):
        rooms = list(self.h.room_set)
        self.assertEqual(2, len(rooms)) 

        self.assertEqual(6, Reservation.all().count()) 

        rs = list(self.h.recently_reservations())
        self.assertEqual(3, len(rs)) 

        # order by chekcin ascendingly
        self.assert_(rs[0].checkin < rs[1].checkin)
        self.assert_(rs[1].checkin < rs[2].checkin)

    def testAvailableRooms(self):
        rs = self.h.available_rooms(yestorday)
        self.assertEqual(0, sum(1 for r in rs))

        rs = self.h.available_rooms(tomorrow)
        self.assertEqual(2, sum(1 for r in rs))

        rs = self.h.available_rooms(today)
        self.assertEqual(1, sum(1 for r in rs))
        rs = self.h.available_rooms(today)
        r = rs.next()
        self.assertEqual(u"四人房", r.name)


    def testIsHoliday(self):
        # test Sat, Sun
        self.assert_(self.h.isholiday(date(2010, 9, 11)))
        self.assert_(self.h.isholiday(date(2010, 9, 12)))
        # test SysHoliday
        self.assert_(self.h.isholiday(date(2010, 7, 29)))
        
        self.failIf(self.h.isholiday(date(2010, 7, 28)))

        # test negtive list
        hd = self.h.holiday_set.\
             filter('date', date(2010, 12, 25)).get()
        self.assertEqual(u"聖誔節不漲價", hd.name)
        self.failIf(self.h.isholiday(date(2010, 12, 25)))

        # test positive list
        self.assert_(self.h.isholiday(date(2010, 7, 23)))
            
    def testPrice(self):
       # self.res3 = Reservation(room=self.room2, 
       #                         name=u"沈懿嬅", 
       #                         checkin=date(2010, 7, 28), 
       #                         checkout=date(2010, 8, 4))
       # self.room2 = Room(homestay=self.h, 
       #                   name=u"四人房", 
       #                   price=2000,
       #                   holiday_price=4000
       #                  )
       #7/29 (4) 4000
       #7/30 (5) 2000
       #7/31 (6) 4000
       #8/1  (7) 4000
       #8/2  (1) 2000
       #8/3  (2) 2000
       #8/4  (3) 2000
       self.assertEqual(20000, 
                        sum(p['value'] for p in self.res3.price_items()))

       self.assertEqual(20000, self.res3.price())
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
