# coding=utf8
from google.appengine.ext import db
from google.appengine.api import users
from datetime import date
from datetime_iterator import datetimeIterator
from datetime import timedelta
import datetime

class Homestay(db.Model):
    name = db.StringProperty(verbose_name="名稱", required=True)
    owner= db.UserProperty(verbose_name="主人",required=True)
    address = db.PostalAddressProperty(verbose_name="地址")
    email = db.EmailProperty(verbose_name="電子信箱")
    blog = db.LinkProperty(verbose_name="民宿網站")

    phone = db.TextProperty(verbose_name="聯絡電話", 
                            default=u'輸入聯絡電話')

    mobile = db.TextProperty(verbose_name="聯絡手機", 
                             default=u'輸入聯絡手機')

    notice = db.TextProperty(verbose_name="訂房注意事項", 
                             default=u'輸入訂房注意事項')

    comment = db.TextProperty(verbose_name="備註", 
                              default=u'輸入備註')

    def daily_books(self, date):
        # analogy for SQL: 
        # select limit 1 Reservations.* 
        # from Homestays, Rooms, Reservations
        # where Homestays.key = :self.key() 
        # and Rooms.homestay = Homestays.key 
        # and Reservations.room = Rooms.key
        # and :date >= Reservations.checkin 
        # and :date < Reservations.checkout
        for r in self.room_set: 
            #bs = r.reservation_set.filter('checkout >', date)
            #bs = [b for b in bs if date >= b.checkin]
            b = r.daily_book(date)
            if b:
                yield b
            else: 
                if date >= date.today():
                    yield r 

    def monthly_books(self, year, month):
        from calendar import Calendar
        c = Calendar()
        m = c.monthdatescalendar(year, month)
        result = []
        for w in m:
            weekly_books = []
            for d in w:
              weekly_books.append({
                  'date':d,
                  'daily_books':self.daily_books(d)
              })
            result.append(weekly_books)
        return result

    
    def recently_reservations(self):
        rooms = list(self.room_set)
        q = Reservation.all().\
                filter("room IN", rooms).\
                filter("checkout >=", date.today())

        def compare(a, b):
            return cmp(a.checkin, b.checkin)
#.sort(compare) 
        a = q.fetch(limit=10)
        a.sort(compare)
        return a

    def daily_availables(self, date):
        if date >= date.today(): 
            return [b for b in self.daily_books(date)
                            if b.kind() == 'Room']
        else:
            return []

    def monthly_availables(self, year, month):
        from calendar import Calendar
        c = Calendar()
        m = c.monthdatescalendar(year, month)
        result = []
        for w in m:
            weekly_availables = []
            for d in w:
              weekly_availables.append({
                 'date':d,
                 'daily_availables':self.daily_availables(d)
               })
            result.append(weekly_availables)
        return result 

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

    def admin_edit_path(self):
        return "/admin/homestay/edit"

    def calendar_path(self):
        return "/admin"
    
    def holidays_path(self):
        return "/admin/holidays"

    def specials_path(self):
        return "/admin/specials"

    def isholiday(self, date):
        # negative list
        if self.holiday_set.\
                filter('date', date).\
                filter('isholiday', False).count() > 0:
            return False

        if date.weekday() in (5, 6) or \
           SysHoliday.all().filter('date', date).count() > 0 \
           or self.holiday_set.\
              filter('date', date).\
              filter('isholiday', True).count() > 0:
               return True

    def holidays(self, date):
        h = SysHoliday.all().filter('date', date).get()
        if h: yield h
        h = self.holiday_set.filter('date', date).get()
        if h: yield h

    def monthly_holidays(self, year, month):
        from calendar import Calendar
        c = Calendar()
        m = c.monthdatescalendar(year, month)
        monthly_holidays = []
        for w in m:
            weekly_holidays = []
            for d in w:
              weekly_holidays.append({
                  'date':d,
                  'holidays':list(self.holidays(d))
              })
            monthly_holidays.append(weekly_holidays)
        return monthly_holidays

    def specials(self, date):
        for r in self.room_set: 
            special = r.special_set.filter('date', date).get()
            if special: yield special # an normal room
            else: yield r # a special room
    
    def monthly_specials(self, year, month):
        from calendar import Calendar
        c = Calendar()
        m = c.monthdatescalendar(year, month)
        monthly_specials = []
        for w in m:
            weekly_specials = []
            for d in w:
              weekly_specials.append({
                  'date':d,
                  'specials':list(self.specials(d))
              })
            monthly_specials.append(weekly_specials)
        return monthly_specials

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

    def daily_book(self, date):
        bs = self.reservation_set.filter('checkout >', date)
        bs = [b for b in bs if date >= b.checkin]
        if len(bs) == 0:
            return None
        else:
            return bs[0]

    def period_books(self, checkin, checkout):
        period = datetimeIterator(checkin, 
                                  checkout - timedelta(days=1))
        bs = []
        for d in period:
            b = self.daily_book(d)
            if b:
                if b.key() not in (b.key() for b in bs):
                    bs.append(b)
        return bs

    def special(self, date):
        s = self.special_set.filter("date", date).get()
        return s 

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
  
    def isholiday(self, date):
        return self.room.homestay.isholiday(date)

    def special(self, date):
        return self.room.special(date)

    def price_items(self):
        from datetime_iterator import datetimeIterator
        from datetime import timedelta 
        for d in datetimeIterator(self.checkin + timedelta(days=1), \
                 self.checkout):
            if self.special(d):
                yield {'date':d, 'value':self.special(d).price}
            elif self.isholiday(d):
                yield {'date':d, 'value':self.room.holiday_price}
            else:
                yield {'date':d, 'value':self.room.price}

    def price(self):
        return sum(p['value'] for p in self.price_items())

    def period_available(self):
        period = datetimeIterator(self.checkin, 
                                  self.checkout - timedelta(days=1))
        for d in period:
            if self.room.daily_book(d):
                return False
        return True

    def period_books(self):
        period = datetimeIterator(self.checkin, 
                                  self.checkout - timedelta(days=1))
        bs = []
        for d in period:
            b = self.room.daily_book(d)
            if b:
                if b.key() not in (b.key() for b in bs):
                    bs.append(b)
        return bs

    def book(self):
        if self.period_available():
            self.put()
        else:
            raise PeriodHasBooksError("period has books")

    def admin_show_path(self):
        return "/admin/%s" % self.key()

    def admin_edit_path(self):
        return "/admin/%s/edit" % self.key()

    def admin_delete_path(self):
        return "/admin/%s/delete" % self.key()

    def admin_calendar_path(self):
        return "/admin/%s" % self.checkin.strftime('%Y%m')

    def client_calendar_path(self):
        return "/%s/%s" % (self.room.homestay.key(), 
                          self.checkin.strftime('%Y%m'))

class Holiday(db.Model):
    homestay = db.ReferenceProperty(Homestay, required=True)
    name = db.StringProperty(verbose_name="名稱", required=True, 
                             default=u'請輸入假日名稱')
    date = db.DateProperty(verbose_name="日期", required=True)
    isholiday = db.BooleanProperty(verbose_name="是否為假期", 
                                   required=True, 
                                   default=True)

    def edit_path(self):
        return "/admin/holidays/%s/edit" % self.key()

    def delete_path(self):
        return "/admin/holidays/%s/delete" % self.key()

    def calendar_path(self):
        return "/admin/holidays/%s" % self.date.strftime('%Y%m')

class SysHoliday(db.Model):
    name = db.StringProperty(verbose_name=u"名稱", required=True)
    date = db.DateProperty(verbose_name=u"假期", required=True)

class Special(db.Model):
    room = db.ReferenceProperty(Room, verbose_name="特假房間", 
                                required=True)

    name = db.StringProperty(verbose_name=u"名稱", required=True, 
                             default="請輸入名稱")

    date = db.DateProperty(verbose_name=u"特假日", required=True)

    price = db.IntegerProperty(verbose_name=u"特價", required=True,
                               default=0)

    def calendar_path(self):
        return "/admin/specials/%s" % self.date.strftime('%Y%m')

class PeriodHasBooksError(Exception):
    pass

def strpdate(str, fmt="%Y%m%d"):
    import time
    d = time.strptime(str, fmt)
    return date(*d[0:3])

def strfdate(d, fmt="%Y%m%d"):
    return date.strftime(d, fmt)
