import sys, os, cgi, re
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.ext.webapp import template
from datetime import date
from model import *

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

class AdminPage(webapp.RequestHandler):
    def homestay(self):
        user = users.get_current_user()
        homestay = Homestay.all().filter('owner', user).get()
        return homestay

    def logout_url(self):
        return users.create_logout_url('/admin')

    def room(self):
        p = r'/admin/(\w+)/\w+'            # pattern
        m = re.match(p, self.request.path) # match
        k = m.group(1)                     # key for homestay
        r = Room.get(k)                    # Room object
        return r

    def date(self):
        p = r'/admin/\w+/(\d+)' 
        m = re.match(p, self.request.path)

        if not m: return date.today()
        d = m.group(1)     
        d = strpdate(d, '%Y%m%d')
        return d

    def month(self):
        p = r'/admin/\w+(/(\d{6}))?'
        m = re.match(p, self.request.path)

        if not m: return date.today()
        if not m.group(2): return date.today()

        d = m.group(2) + '01'     
        d = strpdate(d)
        return d

class IndexPage(AdminPage):
    def get(self):
        homestay = self.homestay()

        if not homestay:
            self.redirect(users.create_logout_url('/admin'))
            return

        month = self.month()
        available_rooms_in_month = homestay.available_rooms_in_month(month.year, month.month)

        template_values = {
            'h': homestay,
            'today':date.today(),
            'month':month,
            'last_month':last_month(month),
            'next_month':next_month(month),
            'available_rooms_in_month':available_rooms_in_month, 
            'logout_url':self.logout_url()
        }

        self.response.out.write(
            template.render('index.html', template_values))

class NewPage(AdminPage):
    def get(self):
        homestay = self.homestay()
        month = self.month()
        template_values = {
            'h': homestay
        }

        self.response.out.write(
            template.render('new.html', template_values))

    def post(self):
        r = self.request
        room = Room(name = r.get("name") ,
                    price = int(r.get("price")), 
                    holiday_price = int(r.get("holiday_price")), 
                    homestay = self.homestay())
        room.put()
        self.redirect('/admin')

class EditPage(AdminPage):
    def get(self):
        room = self.room()
        template_values = {
            'h': room.homestay, 
            'r': room
        }
        self.response.out.write(
            template.render('edit.html', template_values))

    def post(self):
        r = self.request
        room = self.room()
        room.name = r.get("name")
        room.price = int(r.get("price"))
        room.holiday_price = int(r.get("holiday_price"))
        room.put()
        self.redirect('/admin')

class DelPage(AdminPage):
    def get(self):
        room = self.room()
        room.delete()
        self.redirect('/admin')

application = webapp.WSGIApplication([
              (r'/admin', IndexPage), 
              (r'/admin/new', NewPage), 
              (r'/admin/\w+/edit', EditPage), 
              (r'/admin/\w+/delete', DelPage)
              ], debug=True)

def main():
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
