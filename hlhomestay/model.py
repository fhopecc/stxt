import datetime
from google.appengine.ext import db
from google.appengine.api import users

class Homestay(db.Model):
    account = db.StringProperty(required=True)
    name = db.StringProperty(required=True)
    owner= db.UserProperty(required=True)
    address = db.PostalAddressProperty()
    email_address = db.EmailProperty()
    blog = db.ListProperty

class Room(db.Model):
    name = db.StringProperty(required=True)
    value = db.IntegerProperty()
    homestay = db.ReferenceProperty(Homestay)
