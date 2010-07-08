import sys, os, cgi, re
from datetime import date
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from lib import template 
from lib import url_handler
from lib.template import Template 
from model import Homestay
import logging

class RoomHandler(webapp.RequestHandler):
    def homestay(self):
        p = r'/homestays/(\w+)(/.*)?' # pattern
        m = re.match(p, self.request.path)    # match
        k = m.group(1)                # key for homestay
        h = Homestay.get(k)           # Homestay object
        return h

class MainPage(RoomHandler):
  def get(self):
        account = get_account(self.request.path)
        homestay = Homestay(account)
        render = template.frender('show.html')
        self.response.out.write(str(render(homestay)))
      
class NewPage(RoomHandler):
    def get(self):
        render = template.frender('new.html')
        self.response.headers['Content-Type'] = 'text/html'
        self.response.out.write(str(render(self.homestay())))

    # create entity
    def post(self):
        r = self.request
        account = r.get("account")
        owner = users.User(r.get("owner"))
        name = r.get("name")
        h = Homestay(account = r.get("account"), 
                     name= r.get("name") ,
                     address=r.get("address"),
                     email=r.get("email"),
                     blog=r.get("blog"),
                     owner=owner)
        h.put()
        self.redirect('homestay/%s' % account)

application = webapp.WSGIApplication(
                                     [('/homestays/\w+/rooms/new', NewPage), 
                                      ('/rooms/\w+', MainPage)
                                     ],
                                     debug=True)

def main():
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
