# coding=utf8
import datetime
from datetime import date
from google.appengine.ext import db
from google.appengine.api import users

class Homestay(db.Model):
    name = db.StringProperty(verbose_name="名稱", required=True)
    owner= db.UserProperty(verbose_name="主人",required=True)
    address = db.PostalAddressProperty(verbose_name="地址")
    email = db.EmailProperty(verbose_name="電子信箱")
    blog = db.LinkProperty(verbose_name="網站")
    phone = db.TextProperty(verbose_name="聯絡電話", 
                            default=u'輸入聯絡電話')

    def available_rooms(self, date):
        if date < datetime.date.today(): return []

        ars = []
        for r in self.room_set: 
            if r.reservation_set.filter('date =', date).count() == 0:
                ars.append(r)
        return ars

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

class Reservation(db.Model):
    name = db.StringProperty(verbose_name="訂戶名稱", 
                             default=u'輸入訂戶名稱', 
                             multiline=False)

    phone = db.TextProperty(verbose_name="聯絡電話", 
                            default=u'輸入聯絡電話')

    email = db.EmailProperty(verbose_name="電子信箱")

    date = db.DateProperty(verbose_name="訂房日期",
                           auto_now_add=True)

    create_date = db.DateProperty(verbose_name="訂單日期",
                           auto_now_add=True)

    comment = db.TextProperty(verbose_name="備註")

    room = db.ReferenceProperty(Room)

def strpdate(str, fmt="%Y%m%d"):
    import time
    d = time.strptime(str, fmt)
    return date(*d[0:3])

def strfdate(d, fmt="%Y%m%d"):
    return date.strftime(d, fmt)
