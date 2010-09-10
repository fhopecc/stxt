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

    def room(self):
        p = r'/admin/(\w+)/\d{8}'   
        m = re.match(p, self.request.path)
        r = m.group(1)                   
        r = Room.get(r)                 
        return r

    def reservation(self):
        p = r'/admin/(\w+)'            # pattern
        m = re.match(p, self.request.path) # match
        k = m.group(1)                     # key for homestay
        r = Reservation.get(k)                    # Room object
        return r


    def logout_url(self):
        return users.create_logout_url('/admin')

    def date(self):
        p = r'/admin/\w+/(\d{8})' 
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

        template_values = {
            'h': homestay,
            'today':date.today(),
            'month':month,
            'last_month':last_month(month),
            'next_month':next_month(month),
            'monthly_rooms_status':
                   homestay.monthly_rooms_status(month.year, month.month), 
            'logout_url':self.logout_url()
        }
        self.response.out.write( template.render('index.html', template_values))

# NewPage for booking a room
class NewPage(AdminPage):
    def get(self):
        r = self.room()
        d = self.date()

        from datetime import timedelta
        res = Reservation(room = r, checkin = d, 
                          checkout = d + timedelta(days=1)
                          )
        
        template_values = {
            'h': res.room.homestay,
            'res': res
        }

        self.response.out.write(template.render('new.html', 
                                template_values))

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

class ShowPage(AdminPage):
   def get(self):
        res = self.reservation()

        template_values = {
            'h': res.room.homestay, 
            'res': res
        }

        self.response.out.write(template.render('show.html', 
                                template_values))

class EditPage(AdminPage):
    def get(self):
        res = self.reservation()

        template_values = {
            'h': res.room.homestay, 
            'res': res
        }
        self.response.out.write(
            template.render('edit.html', template_values))

    def post(self):
        r = self.request

        res = self.reservation()

        res.name  = r.get('name')
        res.phone = db.PhoneNumber(r.get('phone'))
        res.email = r.get('email') 
        res.checkin = strpdate(r.get('checkin'))
        res.checkout = strpdate(r.get('checkout')) 
        res.create_date = strpdate(r.get('create_date'))
        res.comment = r.get('comment') 
        res.room = Room.get(r.get('room'))
        res.put()

        template_values = {
            'h': res.room.homestay, 
            'res': res
        }

        self.response.out.write(template.render('show.html', 
                                template_values))

class DelPage(AdminPage):
    def get(self):
        res = self.reservation()
        res.delete()
        self.redirect('/admin')

class AdminRoomPage(AdminPage):
    def room(self):
        p = r'/admin/room/(\w+)/\w+'       # pattern
        m = re.match(p, self.request.path) # match
        k = m.group(1)                     # key for homestay
        r = Room.get(k)                    # Room object
        return r

class RoomNewPage(AdminRoomPage):
    def get(self):
        homestay = self.homestay()
        template_values = {
            'h': homestay
        }
        self.response.out.write(
            template.render('newroom.html', template_values))

    def post(self):
        r = self.request
        room = Room(name = r.get("name") ,
                    price = int(r.get("price")), 
                    holiday_price = int(r.get("holiday_price")), 
                    homestay = self.homestay())
        room.put()
        self.redirect('/admin')

class RoomEditPage(AdminRoomPage):
    def get(self):
        room = self.room()
        template_values = {
            'h': room.homestay, 
            'r': room
        }
        self.response.out.write(
            template.render('editroom.html', template_values))

    def post(self):
        r = self.request
        room = self.room()
        room.name = r.get("name")
        room.price = int(r.get("price"))
        room.holiday_price = int(r.get("holiday_price"))
        room.put()
        self.redirect('/admin')

class RoomDelPage(AdminRoomPage):
    def get(self):
        room = self.room()
        room.delete()
        self.redirect('/admin')

application = webapp.WSGIApplication([
               (r'/admin', IndexPage), 
               (r'/admin/room/new', RoomNewPage), 
               (r'/admin/room/\w+/edit', RoomEditPage), 
               (r'/admin/room/\w+/delete', RoomDelPage), 
               (r'/admin/\w+', ShowPage),
               (r'/admin/\w+/edit', EditPage),
               (r'/admin/\w+/delete', DelPage), 
               (r'/admin/\w+/\d{8}', NewPage), 
              ], debug=True)

def main():
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
