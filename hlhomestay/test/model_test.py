# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from google.appengine.api import users
from django.utils import simplejson
from model import *
from datetime import date
from datetime import timedelta
from datetime_iterator import datetimeIterator
import datetime
# before must be zero
today = date.today()
yestorday = date.today() - timedelta(days=1)
twodago = date.today() - timedelta(days=2)
tomorrow = date.today() + timedelta(days=1)
twodafter = date.today() + timedelta(days=2)
nextweek = date.today() + timedelta(days=7)

class UnitTest(unittest.TestCase):
    def setUp(self):
        '''
        訂房檔
        ------
        物件 民宿     房間   房型   房客     加床數 入住日期 退房日期
        b1   五餅二魚 聽濤   雙人房 張簡稜剛 0      前天     昨天
        b2   五餅二魚 松濤   四人房 陳堂山   0      前天     今天
        b3   五餅二魚 迎曦   四人房 沈懿媗   0      前天     後天   
        b4   五餅二魚 聽濤   雙人房 張簡金水 0      今天     明天
        b5   五餅二魚 松濤   四人房 沈懿媗   0      今天     後天
        b6   五餅二魚 多人房 多人房 沈懿嬅   1      明天     後天
        b7   五餅二魚 聽濤   四人房 張簡稜剛 0      明天     下星期
        '''
        owner = users.User("test@gmail.com")

        self.h1 = Homestay(name=u"五餅二魚", 
                           owner=owner)
        self.h1.put()

        self.p1 = PriceType(name=u"雙人房",
                            price=2000,
                            holiday_price=2500, 
                            homestay=self.h1
                           )
        self.p1.put()

        self.p2 = PriceType(name=u"四人房",
                            price=2500,
                            holiday_price=3000,
                            homestay=self.h1
                           )
        self.p2.put()

        self.p3 = PriceType(name=u"多人房",
                            price=2500,
                            holiday_price=3000,
                            bed_price=400,
                            homestay=self.h1
                           )

        self.p3.put()

        self.r1 = Room(homestay=self.h1, name=u"聽濤")
        self.r1.price_type_keys.append(self.p1.key())
        self.r1.price_type_keys.append(self.p2.key())

        self.r1.put()

        self.b1 = Reservation(name=u"張簡稜剛",
                              checkin=twodago, 
                              checkout=yestorday, 
                              room=self.r1,
                              price_type=self.p1
                             )
        self.b1.put()

        self.b4 = Reservation(name=u"張簡金水",
                              checkin=today, 
                              checkout=tomorrow,
                              room=self.r1,
                              price_type=self.p1
                             )
        self.b4.put()

        self.b7 = Reservation(name=u"張簡稜剛",
                              checkin=tomorrow, 
                              checkout=nextweek, 
                              room=self.r1,
                              price_type=self.p2
                             )
        self.b7.put()

        self.r2 = Room(homestay=self.h1, 
                       name=u"松濤"
                      )
        self.r2.price_type_keys.append(self.p1.key())
        self.r2.price_type_keys.append(self.p2.key())
        self.r2.put()



        self.b2 = Reservation(name=u"陳堂山",
                              checkin=twodago, 
                              checkout=today,
                              room=self.r2,
                              price_type=self.p2
                              )
        self.b2.put()

        self.b5 = Reservation(name=u"沈懿媗", 
                              checkin=today,
                              checkout=twodafter,
                              room=self.r2,
                              price_type=self.p2
                             )
        self.b5.put()

        self.r3 = Room(homestay=self.h1, 
                       name=u"迎曦"
                      )
        self.r3.price_type_keys.append(self.p1.key())
        self.r3.price_type_keys.append(self.p2.key())
        self.r3.put()

        self.b3 = Reservation(name=u"沈懿媗", 
                              checkin=twodago, 
                              checkout=twodafter,
                              room=self.r3,
                              price_type=self.p2
                             )
        self.b3.put()

        self.r4 = Room(homestay=self.h1, 
                       name=u"多人房"
                      )
        self.r4.price_type_keys.append(self.p3.key())
        self.r4.put()

        self.b6 = Reservation(name=u"沈懿嬅", 
                              checkin=tomorrow, 
                              checkout=twodafter,
                              room=self.r4,
                              price_type=self.p3
                             )
        self.b6.put()

        '''
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
        物件 民宿     房型   日期         名稱         
        s1   五餅二魚 四人房 下星期       下星期春節4000
        '''


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

        self.s1 = Special(price_type=self.p2,
                          name = u"下星期春節", 
                          date = nextweek,
                          price = 4500
                         )
        self.s1.put()

    def tearDown(self):
        for h in Homestay.all():
            for r in h.room_set:
                for res in r.reservation_set:
                    res.delete()
                r.delete()
            for pt in h.pricetype_set:
                pt.delete()
            h.delete()

    def testRoom(self):
        rooms = self.h1.room_set
        self.assertEqual(4, rooms.count())

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

    def testDailyBook(self):

        b = self.r1.daily_book(twodago)
        self.assertEqual(u'張簡稜剛', b.name)

        b = self.r1.daily_book(yestorday)
        self.assertEqual(None, b)

        b = self.r1.daily_book(today)
        self.assertEqual(u'張簡金水', b.name)

        b = self.r1.daily_book(tomorrow)
        self.assertEqual(u'張簡稜剛', b.name)

        b = self.r1.daily_book(nextweek - timedelta(days=1))
        self.assertEqual(u'張簡稜剛', b.name)

        b = self.r1.daily_book(nextweek)
        self.assertEqual(None, b)

        b = self.r2.daily_book(yestorday)
        self.assertEqual(u'陳堂山', b.name)

        b = self.r2.daily_book(today)
        self.assertEqual(u'沈懿媗', b.name)

        b = self.r2.daily_book(tomorrow)
        self.assertEqual(u'沈懿媗', b.name)

        b = self.r2.daily_book(twodafter)
        self.assertEqual(None, b)


    def testDailyBooks(self):
        # if before today then no availables but books
        bs = self.h1.daily_books(twodago)
        self.assertEqual(3, sum(1 for b in bs))
        
        bs = self.h1.daily_books(yestorday)
        self.assertEqual(2, sum(1 for b in bs))

        # availables + books
        bs = self.h1.daily_books(today)
        self.assertEqual(4, sum(1 for b in bs))


        # booking is order by room order
        bs = self.h1.daily_books(today)

        r = bs.next()
        self.assertEqual('Reservation', r.kind()) 
        self.assertEqual(u'張簡金水', r.name) 
        self.assertEqual(u"聽濤", r.room.name)

        r = bs.next()
        self.assertEqual('Reservation', r.kind()) 
        self.assertEqual(u'沈懿媗', r.name) 
        self.assertEqual(u"松濤", r.room.name)

        r = bs.next()
        self.assertEqual('Reservation', r.kind()) 
        self.assertEqual(u'沈懿媗', r.name) 
        self.assertEqual(u"迎曦", r.room.name)

        r = bs.next()
        self.assertEqual('Room', r.kind()) 
        self.assertEqual(u"多人房", r.name)

        # tomorrow
        bs = self.h1.daily_books(tomorrow)
        self.assertEqual(4, sum(1 for r in bs))

        bs = self.h1.daily_books(tomorrow)

        r = bs.next()
        self.assertEqual('Reservation', r.kind()) 
        self.assertEqual(u'張簡稜剛', r.name) 
        self.assertEqual(u"聽濤", r.room.name)

        r = bs.next()
        self.assertEqual('Reservation', r.kind()) 
        self.assertEqual(u'沈懿媗', r.name) 
        self.assertEqual(u"松濤", r.room.name)

        r = bs.next()
        self.assertEqual('Reservation', r.kind()) 
        self.assertEqual(u'沈懿媗', r.name) 
        self.assertEqual(u"迎曦", r.room.name)

        r = bs.next()
        self.assertEqual('Reservation', r.kind()) 
        self.assertEqual(u'沈懿嬅', r.name) 
        self.assertEqual(u"多人房", r.room.name)

    def testAvailables(self):
        # if date is before today, then no available.
        avs = self.h1.daily_availables(yestorday)
        self.assertEqual(0, sum(1 for r in avs))

        # today 
        avs = self.h1.daily_availables(today)
        self.assertEqual(1, len(avs))

        # tomorrow 
        avs = self.h1.daily_availables(tomorrow)
        self.assertEqual(0, sum(1 for r in avs))

        # nextweek - 1 
        avs = self.h1.daily_availables(nextweek - timedelta(days=1))
        self.assertEqual(3, sum(1 for r in avs))

        # nextweek
        avs = self.h1.daily_availables(nextweek)
        self.assertEqual(4, sum(1 for r in avs))

    def testAvailablePriceTypes(self):
        # if date is before today, then no available.
        avs = self.h1.daily_available_price_types(yestorday)
        self.assertEqual(0, sum(1 for r in avs))

        # today 
        avs = self.h1.daily_available_price_types(today)
        self.assertEqual(1, len(avs))
        self.assertEqual(u'多人房', avs[0].name)

        # tomorrow 
        avs = self.h1.daily_available_price_types(tomorrow)
        self.assertEqual(0, sum(1 for r in avs))

        # nextweek - 1 
        avs = self.h1.daily_available_price_types(nextweek - timedelta(days=1))
        self.assertEqual(3, sum(1 for r in avs))

        # nextweek
        avs = self.h1.daily_available_price_types(nextweek)
        self.assertEqual(3, sum(1 for r in avs))

    def testPeriodAvailable(self):
        # today
        b = Reservation(room=self.r4, name=u"訂房期間測試",
                        checkin=today,
                        checkout=tomorrow)

        self.assert_(b.period_available())

        # tomorrow
        b = Reservation(room=self.r2, name=u"訂房期間測試",
                        checkin=twodafter,
                        checkout=twodafter + timedelta(days=2))
        self.assert_(b.period_available())

        b = Reservation(room=self.r1, name=u"訂房期間測試",
                        checkin=today,
                        checkout=tomorrow + timedelta(days=2))

        self.failIf(b.period_available())
        self.assertRaises(PeriodHasBooksError, b.put)

    def testRecentlyBookings(self):
        bs = self.h1.recently_bookings

        self.assertEqual(6, len(bs)) 

        #import sys, pdb
        #for attr in ('stdin', 'stdout', 'stderr'):
        #    setattr(sys, attr, getattr(sys, '__%s__' % attr))
        #pdb.set_trace()

        # order by chekcin ascendingly
        self.assert_(bs[0].checkin <= bs[1].checkin)
        self.assert_(bs[1].checkin <= bs[2].checkin)

    def testNodepositBookings(self):
        nbs = self.h1.nodeposit_bookings
        self.assertEqual(6, len(nbs))

        self.b7.deposit = 1000
        self.b7.put()
        self.assertEqual(5, len(self.h1.nodeposit_bookings))

    def testMonthlyBooks(self):
        mbs = self.h1.monthly_books(today.year, today.month)
        for w in mbs:
            for d in w:
                if d['date'] == today:
                    for r in d['daily_books']:
                        if r.kind() == 'Reservation':
                            self.assert_(r.name in [u'沈懿媗',
                                                    u'張簡金水'])
                        elif r.kind() == 'Room':
                            self.assertEqual(u'多人房', r.name)


    def testPrice(self):
        #b1   五餅二魚 聽濤   雙人房 張簡稜剛 0      前天     昨天
        #b2   五餅二魚 松濤   四人房 陳堂山   0      前天     今天
        #b3   五餅二魚 迎曦   四人房 沈懿媗   0      前天     後天   
        #b4   五餅二魚 聽濤   雙人房 張簡金水 0      今天     明天
        #b5   五餅二魚 松濤   四人房 沈懿媗   0      今天     後天
        #b6   五餅二魚 多人房 多人房 沈懿嬅   1      明天     後天
        #b7   五餅二魚 聽濤   四人房 張簡稜剛 0      明天     下星期
        pis = self.b1.price_items()

        pi = pis.next() # twodago
        self.assertEqual(twodago, pi['date'])
        if self.h1.isholiday(yestorday):
            self.assertEqual(2500, pi['price'])
        else:
            self.assertEqual(2000, pi['price'])

        self.assertRaises(StopIteration, pis.next)

        pis = self.b3.price_items()

        pi = pis.next() # twodago
        self.assertEqual(twodago, pi['date'])
        if self.h1.isholiday(yestorday):
            self.assertEqual(3000, pi['price'])
        else:
            self.assertEqual(2500, pi['price'])

        pi = pis.next() # yestorday
        self.assertEqual(yestorday, pi['date'])
        if self.h1.isholiday(today):
            self.assertEqual(3000, pi['price'])
        else:
            self.assertEqual(2500, pi['price'])

        pi = pis.next() # today
        self.assertEqual(today, pi['date'])
        if self.h1.isholiday(tomorrow):
            self.assertEqual(3000, pi['price'])
        else:
            self.assertEqual(2500, pi['price'])

        pi = pis.next() # tomorrow
        self.assertEqual(tomorrow, pi['date'])
        if self.h1.isholiday(twodafter):
            self.assertEqual(3000, pi['price'])
        else:
            self.assertEqual(2500, pi['price'])

        self.assertRaises(StopIteration, pis.next)

        self.assertEqual(sum(p['price'] + p['bed_price'] 
                             for p in self.b3.price_items()),
                         self.b3.price())

    def testAddBeds(self):
        # 多人房 2500 3000 400 0.9
        # b6   五餅二魚 多人房 多人房 沈懿嬅   1      明天     後天

        # 加床數不可超過三
        b = self.b6
        b.addbeds_num = 5
        self.assertRaises(BookingError, b.put)

        b.addbeds_num = 3
        b.put()

        pis = self.b6.price_items()
        p = pis.next()
        self.assertEqual(p['price'], 
                        (p['room_price'] + p['bed_price']))

    def testDiscount(self):
        # b5   五餅二魚 松濤   四人房 沈懿媗   0      今天     後天
        self.p2.discount = 0.9

        pis = self.b5.price_items()
        p = pis.next()
        self.assertEqual(p['price'], 
                        (p['room_price'] + p['bed_price']))
        p = pis.next()
        self.assertEqual(p['price'], 
                        (p['room_price'] + p['bed_price']) * 0.9)


    def testSpecial(self):
        
        s = self.p2.special(nextweek)
        
        self.assertEqual(4500, s.price)

        s = self.p2.special(nextweek + timedelta(days=2))

        self.assertEqual(None, s)

        # b7   五餅二魚 聽濤   四人房 張簡稜剛 0      明天     下星期
        # 
        s = self.b7.special(nextweek)
        
        self.assertEqual(4500, s.price)

        pis = self.b7.price_items()

        pi = pis.next() # tomorrow
        self.assertEqual(tomorrow, pi['date'])
        if self.h1.isholiday(tomorrow+timedelta(days=1)):
            self.assertEqual(3000, pi['price'])
        else:
            self.assertEqual(2500, pi['price'])

        pi = pis.next() # 2
        pi = pis.next() # 3
        pi = pis.next() # 4
        pi = pis.next() # 5
        pi = pis.next() # 6
        self.assertEqual(4500, pi['price'])
        self.assertRaises(StopIteration, pis.next) # 7 checkout

    def testRoomPriceType(self):
        self.assertEqual(2, len(self.r1.price_types))
        self.assertEqual(1, len(self.r4.price_types))

        self.assertEqual(3, len(self.p1.rooms))
        self.assertEqual(3, len(self.p2.rooms))
        self.assertEqual(1, len(self.p3.rooms))
