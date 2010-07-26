import sys, os, cgi, re
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from lib import template 
from lib.template import Template 
from model import Homestay
from model import Room
from model import Reservation
from calendar import Calendar
from datetime import date
from model import strpdate
import logging

globals = {"Calendar":Calendar, 
           "today":date.today, 
           "strftime":date.strftime
          }

class ClientPage(webapp.RequestHandler):
    def homestay(self):
        p = r'/(\w+)'
        m = re.match(p, self.request.path)
        k = m.group(1)
        h = Homestay.get(k)
        return h

    def room(self):
        p = r'/(\w+)/\d+'          # pattern
        m = re.match(p, self.request.path) # match
        k = m.group(1)                     # key for homestay
        r = Room.get(k)                    # Room object
        return r

    def date(self):
        p = r'/\w+/(\d+)' 
        m = re.match(p, self.request.path)
        d = m.group(1)     
        d = strpdate(d, '%Y%m%d')
        return d

    def month(self):
        p = r'/\w+(/(\d{6}))?'
        m = re.match(p, self.request.path)

        if not m.group(2): return date.today()

        d = m.group(2) + '01'     
        d = strpdate(d)
        return d

class IndexPage(ClientPage):
    def get(self):
        import datetime
        homestay = self.homestay()
        month = self.month()
        #month = datetime.date.today()
        render = template.frender('index.html', globals=globals)
        self.response.out.write(str(render(homestay, month)))

class ShowPage(ClientPage):
  def get(self):
        homestay = self.homestay()
        render = template.frender('show.html', globals=globals)
        self.response.out.write(str(render(homestay)))

class EditPage(webapp.RequestHandler):
    def get(self):
        render = template.frender('edit.html')
        key = get_key(self.request.path)
        homestay = Homestay.get(key)
        self.response.out.write(str(render(Homestay, homestay)))

    # update entity
    def post(self):
        r = self.request
        h = Homestay.get(r.get("key"))
        h.name = r.get("name")
        h.address = r.get("address")
        h.email = r.get("email")
        h.blog = r.get("blog")
        h.owner = users.User(r.get("owner"))
        h.put()
        self.redirect('%s' % h.key())

class NewPage(ClientPage):
    def get(self):
        r = self.request
        reservation = Reservation()
        reservation.room = self.room()
        reservation.date = self.date()
        
        render = template.frender('new.html')
        self.response.out.write(str(render(reservation)))

    # create entity
    def post(self):
        r = self.request
        reservation = Reservation(name = r.get('name') ,
                        phone = PhoneNumber(r.get('phone')), 
                        email = r.get('email'), 
                        date = strpdate(r.get('date')), 
                        create_date = strpdate(r.get('create_date')),
                        comment = r.get('comment'), 
                        room = self.room())
        reservation.put()

        render = template.frender('order.html')
        self.response.out.write(str(render(reservation)))

class DelPage(webapp.RequestHandler):
    def get(self):
        key = get_key(self.request.path)
        homestay = Homestay.get(key)
        homestay.delete()
        self.redirect('/homestays')

application = webapp.WSGIApplication(
        [
         ('/\w+', IndexPage),
         ('/\w+/\d{6}', IndexPage),
         ('/\w+/\d{8}', NewPage)
        ], debug=True)

def main(): run_wsgi_app(application)

if __name__ == "__main__": main()
