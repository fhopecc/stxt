import sys, os, cgi, re
from datetime import date
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from lib import template 
from lib.template import Template 
from model import Homestay
from model import Room
import logging

globals = {"open":open}

class RoomHandler(webapp.RequestHandler):
    def homestay(self):
        p = r'/homestays/(\w+)(/.*)?' # pattern
        m = re.match(p, self.request.path)    # match
        k = m.group(1)                # key for homestay
        h = Homestay.get(k)           # Homestay object
        return h

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

class MainPage(RoomHandler):
  def get(self):
        room = self.room()
        render = template.frender('show.html')
        self.response.out.write(str(render(room)))
      
class NewPage(RoomHandler):
    def get(self):
        room = Room()
        room.homestay = self.homestay()

        render = template.frender('new.html', globals = globals)
        self.response.out.write(str(render(room)))

    # create entity
    def post(self):
        r = self.create_room()
        r.put()
        self.redirect('/homestays/%s' % r.homestay.key())

class EditPage(RoomHandler):
    def get(self):
        room = self.room()
        render = template.frender('edit.html')
        self.response.out.write(str(render(room)))

    # update entity
    def post(self):
        r = self.update_room()
        r.put()
        self.redirect('/homestays/%s' % r.homestay.key())

class DelPage(RoomHandler):
    def get(self):
        room = self.room()
        room.delete()
        self.redirect('/homestays/%s' % room.homestay.key())

application = webapp.WSGIApplication(
                                     [('/homestays/\w+/rooms/new', NewPage), 
                                      ('/rooms/\w+', MainPage), 
                                      ('/rooms/\w+/edit', EditPage), 
                                      ('/rooms/\w+/delete', DelPage)
                                     ],
                                     debug=True)

def main():
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
