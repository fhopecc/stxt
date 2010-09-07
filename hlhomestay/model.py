# coding=utf8
from google.appengine.ext import db
from google.appengine.api import users
from datetime import date
import datetime

class Homestay(db.Model):
    name = db.StringProperty(verbose_name="名稱", required=True)
    owner= db.UserProperty(verbose_name="主人",required=True)
    address = db.PostalAddressProperty(verbose_name="地址")
    email = db.EmailProperty(verbose_name="電子信箱")
    blog = db.LinkProperty(verbose_name="網站")
    phone = db.TextProperty(verbose_name="聯絡電話", 
                            default=u'輸入聯絡電話')

    def rooms_status(self, date):
        # analogy for SQL: 
        # select * 
        # from Homestays, Rooms, Reservations
        # where Homestays.key = :self.key() 
        # and Rooms.homestay = Homestays.key 
        # and Reservations.room = Rooms.key
        # and :date between Reservations.checkin 
        #               and Reservations.checkout
        if date >= datetime.date.today(): 
            for r in self.room_set: 
                ress = r.reservation_set.filter('checkin <=', date)
                ress = [ res for res in ress if date < res.checkout ]
                if len(ress) == 0: yield r # an available room
                else: yield res # a room reservation

    def monthly_rooms_status(self, year, month):
        from calendar import Calendar
        c = Calendar()
        m = c.monthdatescalendar(year, month)
        monthly_rooms = []
        for w in m:
            weekly_rooms = []
            for d in w:
              weekly_rooms.append({
                  'date':d,
                  'rooms_status':self.available_rooms(d)
              })
            monthly_rooms.append(weekly_rooms)
        return monthly_rooms

    def available_rooms(self, date):
        if date >= datetime.date.today(): 
            for r in self.room_set: 
                # The mapping sql meaning
                # select * from Reservations
                # where room = :r.key()
                # and :date between checkin and checkout
                # import pdb; pdb.set_trace()
                #print r.key()
                #print r.reservation_set.count()
                #print r.reservation_set.get().checkin
                #print date
                ress = r.reservation_set.filter('checkin <=', date)
                #print ress.count()
                ress = [ res for res in ress if date < res.checkout]
                #print len(ress)
                if len(ress) == 0: 
                    yield r # this is an available room

    def available_rooms_in_month(self, year, month):
        from calendar import Calendar
        c = Calendar()
        m = c.monthdatescalendar(year, month)
        rooms_in_month = []
        for w in m:
            rooms_in_week = []
            for d in w:
              rooms_in_week.append({
                 'date':d,
                 'available_rooms':self.available_rooms(d)
               })
            rooms_in_month.append(rooms_in_week)
        return rooms_in_month 

    def next_month_index_path(self, month):
        t = month
        n = t.month + 1 # for next month
        if n > 13:
            n = date(t.year+1, 1, 1)
        else:
            n = date(t.year, n, 1)
        return '/%s/%s' % (self.key(), strfdate(n, '%Y%m'))

    def last_month_index_path(self, month):
        t = month
        l = t.month - 1 # for next month
        if l == 0:
            l = date(t.year-1, 12, 1)
        else:
            l = date(t.year, l, 1)
        return '/%s/%s' % (self.key(), strfdate(l, '%Y%m'))


class Room(db.Model):
    name = db.StringProperty(
            verbose_name="客房名稱", 
            default=u'輸入客房名稱', multiline=False)
    price = db.IntegerProperty(
            verbose_name = "平日價格", 
            default=0)
    holiday_price = db.IntegerProperty(
            verbose_name = "假日價格", 
            default=0) 
    homestay = db.ReferenceProperty(Homestay)

    def book_path(self, date):
        return '/%s/%s' % (self.key(), date.strftime('%Y%m%d'))

class Reservation(db.Model):
    name = db.StringProperty(verbose_name="訂戶名稱", 
                             default=u'輸入訂戶名稱', 
                             multiline=False)

    phone = db.TextProperty(verbose_name="聯絡電話", 
                            default=u'輸入聯絡電話')

    email = db.EmailProperty(verbose_name="電子信箱",
                             default=u'請輸入電子信箱')

    checkin = db.DateProperty(verbose_name="入住日期",
                              auto_now_add=True)

    checkout = db.DateProperty(verbose_name="退房日期",
                               auto_now_add=True)

    create_date = db.DateProperty(verbose_name="訂單日期",
                                  auto_now_add=True)

    comment = db.TextProperty(verbose_name="備註",
                              default=u'請輸入備註')

    room = db.ReferenceProperty(Room)

def strpdate(str, fmt="%Y%m%d"):
    import time
    d = time.strptime(str, fmt)
    return date(*d[0:3])

def strfdate(d, fmt="%Y%m%d"):
    return date.strftime(d, fmt)
