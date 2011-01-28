# coding=utf8
from google.appengine.api import mail
import datetime
mon =  datetime.date.today().month - 1
notice = "請記得執行%d月機時報表程式" % mon
mail.send_mail(sender="fhopecc@gmail.com",
              to="hnet01@mail.hltb.gov.tw",
              subject= notice,
              body= notice)
