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

    css = db.TextProperty(verbose_name="CSS 檔案",
                          default="homestay") 

    ditems_num = db.IntegerProperty(verbose_name="訂房網頁顯示筆數", 
                          default=5) 

    @property
    def price_types(self):
        return self.pricetype_set.fetch(1000)

    @property
    def rooms_price_types(self):
        rooms = self.room_set.fetch(1000)
        price_types = self.pricetype_set.fetch(1000)
        return map(None, rooms, price_types)

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
        rooms = self.room_set.fetch(1000)
        q = Reservation.all().\
                filter("room IN", rooms).\
                filter("checkout >=", date.today())

        def compare(a, b):
            return cmp(a.checkin, b.checkin)
#.sort(compare) 
        a = q.fetch(1000)
        a.sort(compare)
        return a

    @property
    def nodeposit_books(self):
        rooms = self.room_set.fetch(1000)
        q = Reservation.all().\
                filter("room IN", rooms).\
                filter("checkout >=", date.today())

        def compare(a, b):
            return cmp(a.checkin, b.checkin)
#.sort(compare) 
        a = q.fetch(1000)
        a.sort(compare)
        return a

    def daily_availables(self, date):
        if date >= date.today(): 
            return [b for b in self.daily_books(date)
                    if b.kind() == 'Room']
        else:
            return []

    def daily_available_price_type_keys(self, date):
        pts = []
        for r in self.daily_availables(date):
            pts.extend(r.price_type_keys)
        return list(set(pts))

    def daily_available_price_types(self, date):
        return [PriceType.get(ptk) for ptk in
                self.daily_available_price_type_keys(date)]

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
                 'daily_availables':self.daily_availables(d)[:self.ditems_num]
               })
            result.append(weekly_availables)
        return result 

    @property
    def next_month_index_path(self, month):
        t = month
        n = t.month + 1 # for next month
        if n > 13:
            n = date(t.year+1, 1, 1)
        else:
            n = date(t.year, n, 1)
        return '/%s/%s' % (self.key(), strfdate(n, '%Y%m'))

    @property
    def last_month_index_path(self, month):
        t = month
        l = t.month - 1 # for next month
        if l == 0:
            l = date(t.year-1, 12, 1)
        else:
            l = date(t.year, l, 1)
        return '/%s/%s' % (self.key(), strfdate(l, '%Y%m'))

    @property
    def admin_edit_path(self):
        return "/admin/homestay/edit"

    @property
    def calendar_path(self):
        return "/admin"
    
    @property
    def holidays_path(self):
        return "/admin/holidays"

    @property
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
        for p in self.price_types: 
            special = p.special_set.filter('date', date).get()
            if special: yield special # an normal room
            else: yield p # a special pricetype
    
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
    name = db.StringProperty(verbose_name="客房名稱", 
            default=u'輸入客房名稱', multiline=False)

    homestay = db.ReferenceProperty(Homestay)

    price_type_keys = db.ListProperty(db.Key)

    @property
    def price_types(self):
        return [PriceType.get(ptk) for ptk in self.price_type_keys]

    @property
    def price_type_checkboxes(self):
        cbs = [] 
        for p in self.homestay.pricetype_set:
            cbs.append({'key':p.key(),
                        'name':p.name,
                        'checked':p.key() in self.price_type_keys
                       })
        return cbs                

    @property
    def edit_path(self):
        return "/admin/room/%s/edit" % self.key()

    @property
    def delete_path(self):
        return "/admin/room/%s/delete" % self.key()

    def daily_book(self, date):
        bs = self.reservation_set.filter('checkout >', date)
        bs = [b for b in bs if date >= b.checkin]
        if len(bs) > 0:
            return bs[0]
        return None

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


    @property
    def book_path(self, date):
        return '/%s/%s' % (self.key(), date.strftime('%Y%m%d'))


class PriceType(db.Model):
    homestay = db.ReferenceProperty(Homestay)

    name = db.StringProperty(
            verbose_name="計價名稱", 
            default=u'輸入計價名稱', multiline=False)

    price = db.IntegerProperty(
            verbose_name = "平日價", 
            default=0)

    holiday_price = db.IntegerProperty(
            verbose_name = "假日價", 
            default=0) 

    bed_price = db.IntegerProperty(
            verbose_name = "加床價", 
            default=0) 

    @property
    def rooms(self):
        return Room.gql('WHERE price_type_keys = :1', \
                        self.key()).fetch(1000)

    @property
    def edit_path(self):
        return "/admin/price_types/%s/edit" % self.key()

    @property
    def delete_path(self):
        return "/admin/price_types/%s/delete" % self.key()

    def special(self, date):
        s = self.special_set.filter("date", date).get()
        return s 

class Reservation(db.Model):
    name = db.StringProperty(
            verbose_name="訂戶名稱", 
            default=u'輸入訂戶名稱', 
            multiline=False)

    phone = db.TextProperty(
            verbose_name="聯絡電話", 
            default=u'輸入聯絡電話')

    email = db.EmailProperty(
            verbose_name="電子信箱",
            default=u'請輸入電子信箱')

    checkin = db.DateProperty(
            verbose_name="入住日期",
            auto_now_add=True)

    checkout = db.DateProperty(
            verbose_name="退房日期",
            auto_now_add=True)

    create_date = db.DateProperty(
            verbose_name="訂單日期",
            auto_now_add=True)

    comment = db.TextProperty(
            verbose_name="備註",
            default=u'請輸入備註')

    addbeds_num = db.IntegerProperty(
            verbose_name="加床數",
            default=0)

    addbeds_num = db.IntegerProperty(
            verbose_name="加床數",
            default=0)

    deposit = db.IntegerProperty(
            verbose_name="訂金",
            default=0)

    room = db.ReferenceProperty(Room)

    price_type = db.ReferenceProperty(PriceType)

    @property
    def homestay(self):
        return self.room.homestay
  
    def isholiday(self, date):
        return self.room.homestay.isholiday(date)

    def special(self, date):
        return self.price_type.special(date)

    def price_items(self):
        from datetime_iterator import datetimeIterator
        from datetime import timedelta 
        for d in datetimeIterator(self.checkin, \
                 self.checkout - timedelta(days=1)):
            item = {'date':d, 
                    'value':self.price_type.price, 
                    'addbeds_num':self.addbeds_num, 
                    'addbed_price': \
                        self.addbeds_num * self.price_type.bed_price}
            if self.special(d + timedelta(days=1)):
                special = self.special(d + timedelta(days=1))
                item['value'] = special.price
                item['addbed_price'] = self.addbeds_num * \
                                       special.bed_price
            elif self.isholiday(d + timedelta(days=1)):
                item['value'] = self.price_type.holiday_price

            #item['value'] += self.room.addbed_price * self.addbeds_num
            yield item

    def price(self):
        return sum(p['value'] + p['addbed_price'] \
                   for p in self.price_items())

    def period_available(self):
        period = datetimeIterator(self.checkin, 
                                  self.checkout - timedelta(days=1))

        for d in period:
            bs = self.room.daily_book(d) 
            if bs and bs.is_saved() \
                  and (not self.is_saved() \
                       or bs.key() != self.key()): # 允許修改訂單本身
                return False
        return True

    def period_books(self):
        period = datetimeIterator(self.checkin, 
                                  self.checkout - timedelta(days=1))
        bs = []
        for d in period:
            b = self.room().daily_book(d)
            if b:
                if b.key() not in (b.key() for b in bs):
                    bs.append(b)
        return bs

    def book(self):
        if self.period_available():
            self.put()
        else:
            raise PeriodHasBooksError("period has books")

    def put(self):
        if self.addbeds_num > 3:
            raise BookingError("add beds' num must <= 3")
        elif not self.period_available():
            raise PeriodHasBooksError("period has books")
        else:
            db.Model.put(self)

    @property
    def admin_show_path(self):
        return "/admin/%s" % self.key()

    @property
    def admin_edit_path(self):
        return "/admin/%s/edit" % self.key()

    @property
    def admin_delete_path(self):
        return "/admin/%s/delete" % self.key()

    @property
    def admin_calendar_path(self):
        return "/admin/%s" % self.checkin.strftime('%Y%m')

    @property
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

    @property
    def edit_path(self):
        return "/admin/holidays/%s/edit" % self.key()

    @property
    def delete_path(self):
        return "/admin/holidays/%s/delete" % self.key()

    @property
    def calendar_path(self):
        return "/admin/holidays/%s" % self.date.strftime('%Y%m')

class SysHoliday(db.Model):
    name = db.StringProperty(verbose_name=u"名稱", required=True)
    date = db.DateProperty(verbose_name=u"假期", required=True)

class Special(db.Model):
    price_type = db.ReferenceProperty(PriceType, 
                                      verbose_name="特價房型", 
                                      required=True)

    name = db.StringProperty(verbose_name=u"名稱", required=True, 
                             default="請輸入名稱")

    date = db.DateProperty(verbose_name=u"特假日", required=True)

    price = db.IntegerProperty(verbose_name=u"特價", required=True,
                               default=0)

    bed_price = db.IntegerProperty(verbose_name=u"特價", required=True,
                                   default=0)
    @property
    def homestay(self):
        return self.price_type.homestay

    @property
    def calendar_path(self):
        return "/admin/specials/%s" % self.date.strftime('%Y%m')

    @property
    def edit_path(self):
        return "/admin/specials/%s/edit" % self.key()
    
    @property
    def delete_path(self):
        return "/admin/specials/%s/delete" % self.key()

class BookingError(Exception):
    pass

class PeriodHasBooksError(BookingError):
    pass

def strpdate(str, fmt="%Y%m%d"):
    import time
    d = time.strptime(str, fmt)
    return date(*d[0:3])

def strfdate(d, fmt="%Y%m%d"):
    return date.strftime(d, fmt)
