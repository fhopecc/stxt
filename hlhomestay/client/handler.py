import sys, os, cgi, re
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.ext.webapp import template
from calendar import Calendar
from datetime import date
from datetime import timedelta
from model import * 
import logging

globals = {"Calendar":Calendar, 
           "today":date.today, 
           "strftime":date.strftime
          }

def last_month(d):
    year = d.year
    month = d.month
    last_month = month - 1
    if last_month < 1:
        last_month = 12
        year -= 1
    return date(year, last_month, 1)

def next_month(d):
    year = d.year
    month = d.month
    next_month = month + 1
    if next_month > 12:
        next_month = 1
        year += 1
    return date(year, next_month, 1)

class ClientPage(webapp.RequestHandler):
    def homestay(self):
        p = r'/(\w+)'
        m = re.match(p, self.request.path)
        k = m.group(1)
        h = Homestay.get(k)
        return h

    def room(self):
        p = r'/(\w+)/\d+'                  # pattern
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
        homestay = self.homestay()
        month = self.month()
        monthly_availables = homestay.monthly_availables(month.year, month.month)

        template_values = {
            'h': homestay,
            'today':date.today(),
            'month':month,
            'last_month':last_month(month),
            'next_month':next_month(month),
            'monthly_availables':monthly_availables
        }

        self.response.out.write(
            template.render('index.html', template_values))

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

# NewPage for booking a room
class NewPage(ClientPage):
    def get(self):
        r = self.request
        reservation = Reservation()
        reservation.room = self.room()
        reservation.checkin = self.date()
        reservation.checkout = self.date() + timedelta(days=1)
        
        template_values = {
            'h': reservation.room.homestay,
            'r': reservation
        }

        self.response.out.write(template.render('new.html', 
                                template_values))

    # create entity
    def post(self):
        r = self.request
        res = Reservation(name = r.get('name') ,
                          phone = db.PhoneNumber(r.get('phone')), 
                          email = r.get('email'), 
                          checkin = strpdate(r.get('checkin')), 
                          checkout = strpdate(r.get('checkout')), 
                          create_date = strpdate(r.get('create_date')),
                          comment = r.get('comment'), 
                          room = self.room())
        res.put()

        template_values = {
            'h': res.room.homestay,
            'res': res
        }

        self.response.out.write(template.render('show.html', 
                                template_values))

class DelPage(webapp.RequestHandler):
    def get(self):
        key = get_key(self.request.path)
        homestay = Homestay.get(key)
        homestay.delete()
        self.redirect('/homestays')

class PeriodBooksPage(webapp.RequestHandler):
    def get(self):
        r = self.request
        room = Room.get(r.get('room'))

        bs = room.period_books(strpdate(r.get('checkin')),
                               strpdate(r.get('checkout')))

        from gaejson import GaeEncoder, json

        self.response.out.write(json.dumps(bs, cls=GaeEncoder))

application = webapp.WSGIApplication(
        [
         (r'/period_books\.json.*', PeriodBooksPage), 
         (r'/\w+', IndexPage),
         (r'/\w+/\d{6}', IndexPage),
         (r'/\w+/\d{8}', NewPage)
        ], debug=True)

def main(): run_wsgi_app(application)

if __name__ == "__main__": main()
