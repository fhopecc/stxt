import datetime
from google.appengine.ext import db
from google.appengine.api import users

class Homestay(db.Model):
    account = db.StringProperty(required=True)
    name = db.StringProperty(verbose_name="民宿名稱", required=True)
    owner= db.UserProperty(required=True)
    address = db.PostalAddressProperty()
    email_address = db.EmailProperty(verbose_name="電子信箱",)
    blog = db.LinkProperty(verbose_name="民宿網址", required=True)

class Room(db.Model):
    name = db.StringProperty(required=True)
    value = db.IntegerProperty()
    homestay = db.ReferenceProperty(Homestay)
