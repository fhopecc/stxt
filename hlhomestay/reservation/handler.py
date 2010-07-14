import sys, re, logging
from datetime import date
from google.appengine.ext import db
from google.appengine.ext.db import PhoneNumber
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from lib import template 
from lib.template import Template 
from model import Reservation
from model import Homestay
from model import Room
from model import strpdate

class ReservationHandler(webapp.RequestHandler):
    def room(self):
        p = r'/rooms/(\w+)(/.*)?'          # pattern
        m = re.match(p, self.request.path) # match
        k = m.group(1)                     # key for homestay
        r = Room.get(k)                    # Room object
        return r

    def create_room(self):
        r = self.request
        room = Room(name = r.get("name") ,
                    price = int(r.get("price")), 
                    holiday_price = int(r.get("holiday_price")), 
                    homestay = self.homestay())
        return room

    def update_room(self):
        r = self.request
        room = self.room()
        room.name = r.get("name")
        room.price = int(r.get("price"))
        room.holiday_price = int(r.get("holiday_price"))
        return room

class MainPage(ReservationHandler):
  def get(self):
        room = self.room()
        render = template.frender('show.html')
        self.response.out.write(str(render(room)))
      
class NewPage(ReservationHandler):
    def get(self):
        reservation = Reservation()
        reservation.room = self.room()
        
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

class EditPage(ReservationHandler):
    def get(self):
        room = self.room()
        render = template.frender('edit.html')
        self.response.out.write(str(render(room)))

    # update entity
    def post(self):
        r = self.update_room()
        r.put()
        self.redirect('/homestays/%s' % r.homestay.key())

class DelPage(ReservationHandler):
    def get(self):
        room = self.room()
        room.delete()
        self.redirect('/homestays/%s' % room.homestay.key())

application = webapp.WSGIApplication(
                     [('/rooms/\w+/reservations/new', NewPage), 
                      ('/rooms/\w+', MainPage), 
                      ('/rooms/\w+/edit', EditPage), 
                      ('/rooms/\w+/delete', DelPage)
                     ], debug=True)

def main():
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
