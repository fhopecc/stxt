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
        '''
        訂房檔
        ------
        物件 民宿   房間   房客     入住日期 退房日期

        b1   民宿一 雙人房 張簡稜剛 前天     昨天
        b3   民宿一 雙人房 陳堂山   昨天     明天
        b4   民宿一 雙人房 張簡稜剛 下星期   下星期又三天
        b2   民宿一 四人房 沈懿嬅   前天     後天
        b5   民宿一 大客房 沈懿嬅   20100728 20100804

        系統假日檔
        ----------
        日期     假日名稱

        20100729 稜剛生日
        20101225 聖誕節

        民宿假日檔
        ----------
        日期     假日名稱         是否為假日

        20100723 品果生日         是
        20101225 今年聖誕節不漲價 否

        特價檔 
        ------
        物件 民宿   房間   日期         名稱         
        s1   民宿一 雙人房 下星期又一天 下星期又一天漲三倍
        '''

        owner = users.User("test@gmail.com")

        self.h1 = Homestay(name=u"民宿一", 
                          owner=owner)
        self.h1.put()

        self.r1 = Room(homestay=self.h1, name=u"雙人房", 
                       price=1000,
                       holiday_price=2000 
                      )

        self.r1.put()

        self.b1 = Reservation(room=self.r1, name=u"張簡稜剛",
                              checkin=yestorday - timedelta(days=1), 
                              checkout=yestorday)
        self.b1.put()

        self.b3 = Reservation(room=self.r1, name=u"陳堂山", 
                              checkin=yestorday, 
                              checkout=tomorrow)
        self.b3.put()


        self.b4 = Reservation(room=self.r1, 
                              name=u"張簡稜剛",
                              checkin=nextweek, 
                              checkout=nextweek + timedelta(days=3))

        self.b4.put()

        self.r2 = Room(homestay=self.h1, 
                       name=u"四人房", 
                       price=2000,
                       holiday_price=4000
                      )
        self.r2.put()

        self.b2 = Reservation(room=self.r2, name=u"沈懿嬅",
                              checkin=yestorday - timedelta(days=1), 
                              checkout=tomorrow + timedelta(days=1))
        self.b2.put()

        self.r3 = Room(homestay=self.h1, 
                       name=u"大客房", 
                       price=2000,
                       holiday_price=4000
                      )
        self.r3.put()

        self.b5 = Reservation(room=self.r2, 
                                name=u"沈懿嬅", 
                                checkin=date(2010, 7, 28), 
                                checkout=date(2010, 8, 4))

        self.b5.put()

        SysHoliday(name=u"稜剛生日",
                   date=date(2010, 7, 29)).put()

        SysHoliday(name=u"聖誕節",
                   date=date(2010, 12, 25)).put()

        # Negative holiday list
        Holiday(homestay= self.h1,
                name=u"聖誔節不漲價", 
                date=date(2010, 12, 25), 
                isholiday=False).put()

        # Positive holiday list
        Holiday(homestay= self.h1,
                name=u"品果生日", 
                date=date(2010, 7, 23), 
                isholiday=True).put()

        self.s1 = Special(room=self.r1,
                          name = u"下星期又一天漲三倍", 
                          date = nextweek + timedelta(days=1),
                          price = 3000 
                         )
        self.s1.put()


    def tearDown(self):
        for h in Homestay.all():
            for r in h.room_set:
                for res in r.reservation_set:
                    res.delete()
                r.delete()
            h.delete()

    def testURLMaker(self):
        self.assertEqual('/admin/%s' % self.b1.key(), 
                         self.b1.admin_show_path())
        
    def testRoom(self):
        rooms = self.h1.room_set
        self.assertEqual(3, rooms.count())

        b = self.r1.daily_book(yestorday)
        self.assertEqual(u'陳堂山', b.name)

        b = self.r1.daily_book(today)
        self.assertEqual(u'陳堂山', b.name)

        b = self.r1.daily_book(tomorrow)
        self.assertEqual(None, b)

        b = self.r1.daily_book(nextweek)
        self.assertEqual(u'張簡稜剛', b.name)

        b = self.r1.daily_book(nextweek + timedelta(days=1))
        self.assertEqual(u'張簡稜剛', b.name)

        b = self.r1.daily_book(nextweek + timedelta(days=2))
        self.assertEqual(u'張簡稜剛', b.name)

        b = self.r1.daily_book(nextweek + timedelta(days=3))
        self.assertEqual(None, b)

    def testReservation(self):
        self.assertEqual(3, self.r1.reservation_set.count())
        r = self.r1.reservation_set.get()

        self.assertEqual(u"雙人房", r.room.name)

    def testDailyBooks(self):
        # yestorday < today, so no availables but books
        bs = self.h1.daily_books(yestorday)
        self.assertEqual(2, sum(1 for b in bs))

        bs = self.h1.daily_books(today)
        self.assertEqual(3, sum(1 for b in bs))

        bs = self.h1.daily_books(today)

        r = bs.next()
        self.assertEqual('Reservation', r.kind()) 
        self.assertEqual(u'陳堂山', r.name) 
        self.assertEqual(u"雙人房", r.room.name)

        r = bs.next()
        self.assertEqual('Reservation', r.kind()) 
        self.assertEqual(u'沈懿嬅', r.name) 
        self.assertEqual(u"四人房", r.room.name)

        bs = self.h1.daily_books(tomorrow)
        self.assertEqual(3, sum(1 for r in bs))

        bs = self.h1.daily_books(tomorrow)
        r = bs.next()
        self.assertEqual('Room', r.kind()) 
        self.assertEqual(u"雙人房", r.name)

        r = bs.next()
        self.assertEqual('Reservation', r.kind()) 
        self.assertEqual(u'沈懿嬅', r.name) 
        self.assertEqual(u"四人房", r.room.name)

        bs = self.h1.daily_books(next2weeks + timedelta(days=3))
        r = bs.next()
        self.assertEqual('Room', r.kind()) 
        self.assertEqual(u"雙人房", r.name)

        r = bs.next()
        self.assertEqual('Room', r.kind()) 
        self.assertEqual(u"四人房", r.name)

    def testMonthlyBooks(self):
        mbs = self.h1.monthly_books(today.year, today.month)
        self.assertEqual(5, len(mbs))
        for w in mbs:
            for d in w:
                if d['date'] == today:
                    for r in d['daily_books']:
                        if r.kind() == 'Reservation':
                            self.assert_(r.name in [u'沈懿嬅', u'陳堂山'])

    def testRecentlyReservations(self):
        bs = list(self.h1.recently_reservations())

        self.assertEqual(3, len(bs)) 

        # order by chekcin ascendingly
        self.assert_(bs[0].checkin < bs[1].checkin)
        self.assert_(bs[1].checkin < bs[2].checkin)

    def testPeriodAvailable(self):
        b = Reservation(room=self.r1, name=u"測區間",
                        checkin=tomorrow,
                        checkout=tomorrow + timedelta(days=2))
        self.assert_(b.period_available())

        b = Reservation(room=self.r1, name=u"測區間",
                        checkin=today,
                        checkout=tomorrow + timedelta(days=2))
        self.failIf(b.period_available())
        self.assertRaises(PeriodHasBooksError, b.book)

    def testAvailables(self):
        avs = self.h1.daily_availables(yestorday)
        self.assertEqual(0, sum(1 for r in avs))

        avs = self.h1.daily_availables(tomorrow)
        self.assertEqual(2, sum(1 for r in avs))

        avs = self.h1.daily_availables(today)
        self.assertEqual(1, len(avs))

    def testIsHoliday(self):
        # test Sat, Sun
        self.assert_(self.h1.isholiday(date(2010, 9, 11)))
        self.assert_(self.h1.isholiday(date(2010, 9, 12)))
        # test SysHoliday
        self.assert_(self.h1.isholiday(date(2010, 7, 29)))
        
        self.failIf(self.h1.isholiday(date(2010, 7, 28)))

        # test negtive list
        hd = self.h1.holiday_set.\
             filter('date', date(2010, 12, 25)).get()

        self.assertEqual(u"聖誔節不漲價", hd.name)
        self.failIf(self.h1.isholiday(date(2010, 12, 25)))

        # test positive list
        self.assert_(self.h1.isholiday(date(2010, 7, 23)))
            
    def testPrice(self):
       # self.b5 = Reservation(room=self.r2, 
       #                         name=u"沈懿嬅", 
       #                         checkin=date(2010, 7, 28), 
       #                         checkout=date(2010, 8, 4))
       # self.r2 = Room(homestay=self.h1, 
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
                        sum(p['value'] for p in self.b5.price_items()))

       self.assertEqual(20000, self.b5.price())

    def testSpecial(self):
        
        s = self.r1.special(nextweek + timedelta(days=1))
        
        self.assertEqual(3000, s.price)

        s = self.r1.special(nextweek + timedelta(days=2))

        self.assertEqual(None, s)

        # self.b4 = Reservation(room=self.r1, 
        #                         name=u"張簡稜剛",
        #                         checkin=nextweek, 
        #                         checkout=nextweek + timedelta(days=3))
        #
        # nextweek(Sat) checkin      Wed      Thu      Fri
        # nextweek(Sun) special 3000 Thu 3000 Fri 3000 Sat 3000
        # nextweek(Mon) special 1000 Fri 1000 Sat 2000 Sun 2000
        # nextweek(Tue) special 1000 Sat 2000 Sun 2000 Mon 1000
        #                       5000     6000     7000     6000

        if nextweek.weekday in(2, 4): # WED, FRI
            self.assertEqual(6000, self.b4.price())
        elif nextweek.weekday == 3: # Thu
            self.assertEqual(7000, self.b4.price())
        else:
            self.assertEqual(5000, self.b4.price())

        specials = self.h1.specials(nextweek + timedelta(days=1))

        self.assertEqual(3, sum(1 for s in specials))

        specials = self.h1.specials(nextweek + timedelta(days=1))

        s = specials.next()

        self.assertEqual('Special', s.kind())

        s = specials.next()

        self.assertEqual('Room', s.kind())

if __name__ == '__main__':
    unittest.main()
    #tests = unittest.TestSuite()
    #tests.addTest(UnitTest("testAvailables"))
    #UnitTest("testAvailables").debug()
    #tests.debug()
    #runner = unittest.TextTestRunner()
    #runner.run(tests)
