import datetime
from google.appengine.ext import db
from google.appengine.api import users
class Homestay(db.Model):
  name  = db.StringProperty(verbose_name="民宿名稱", required=True)
  link = db.LinkProperty(verbose_name="民宿網址", required=True)
  email = db.EmailProperty(verbose_name="電子信箱", required=True)
  status = db.StringProperty(verbose_name="帳號狀態",
      required=True, 
      default="invalid", 
      choices=set(["valid", "invalid"]))
  create_at = db.DateProperty(auto_now_add=True)
