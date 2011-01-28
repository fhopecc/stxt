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
    @property
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
        homestay = self.homestay
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
          homestay = self.homestay
          render = template.frender('show.html', globals=globals)
          self.response.out.write(str(render(homestay)))

# NewPage for booking a room
class NewPage(ClientPage):
    def get(self):
        r = self.request
        room = self.room()
        res = Reservation()
        res.checkin = self.date()
        res.checkout = self.date() + timedelta(days=1)
        
        template_values = {
            'h': room.homestay,
            'r': room,
            'one_price': len(room.price_types) == 1,
            'res': res
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
                          addbeds_num = int(r.get('addbeds_num')), 
                          comment = r.get('comment'), 
                          room = Room.get(r.get('room')),
                          price_type = PriceType.get(r.get('price_type'))
                         )
        res.put()

        template_values = {
            'h': res.homestay,
            'res': res
        }

        self.response.out.write(template.render('show.html', 
                                template_values))

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
         (r'/new', NewPage),
         (r'/\w+', IndexPage),
         (r'/\w+/\d{6}', IndexPage),
         (r'/\w+/\d{8}', NewPage)
        ], debug=True)

def main(): run_wsgi_app(application)

if __name__ == "__main__": main()
