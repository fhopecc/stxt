# coding=utf8
import datetime
from google.appengine.ext import db
from google.appengine.api import users

class Homestay(db.Model):
    name = db.StringProperty(verbose_name="名稱", required=True)
    owner= db.UserProperty(verbose_name="主人",required=True)
    address = db.PostalAddressProperty(verbose_name="地址",)
    email = db.EmailProperty(verbose_name="電子信箱",)
    blog = db.LinkProperty(verbose_name="網站", required=True)

class Room(db.Model):
    name = db.StringProperty(required=True)
    price = db.IntegerProperty()
    homestay = db.ReferenceProperty(Homestay)
