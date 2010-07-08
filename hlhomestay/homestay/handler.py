import sys, os, cgi, re
from datetime import date
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from lib import template 
from lib.template import Template 
from model import Homestay
import logging

class HomestayPage(webapp.RequestHandler):
    def homestay_key():
        p = r'/homestays/(\w+)(/.*)?' # pattern
        m = re.match(p, self.path) # match
        return m.group(1)

class IndexPage(webapp.RequestHandler):
    def get(self):
        homestays = Homestay.all()
        render = template.frender('index.html')
        self.response.out.write(str(render(Homestay, homestays)))

class ShowPage(webapp.RequestHandler):
  def get(self):
        key = get_key(self.request.path)
        homestay = Homestay.get(key)
        render = template.frender('show.html')
        self.response.out.write(str(render(Homestay, homestay)))


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

class NewPage(webapp.RequestHandler):
    def get(self):
        render = template.frender('new.html')
        self.response.out.write(str(render(Homestay)))

    # create entity
    def post(self):
        r = self.request
        account = r.get("account")
        owner = users.User(r.get("owner"))
        name = r.get("name")
        h = Homestay(account = r.get("account"), 
                     name= r.get("name") ,
                     address= r.get("address"),
                     email=r.get("email"),
                     blog=r.get("blog"),
                     owner=owner)
        h.put()
        self.redirect('%s' % h.key())

class DelPage(webapp.RequestHandler):
    def get(self):
        key = get_key(self.request.path)
        homestay = Homestay.get(key)
        homestay.delete()
        self.redirect('/homestays')

application = webapp.WSGIApplication(
        [
         ('/homestays', IndexPage), 
         ('/homestays/new', NewPage), 
         ('/homestays/\w+/edit', EditPage), 
         ('/homestays/\w+/delete', DelPage), 
         ('/homestays/edit', EditPage), 
         ('/homestays/\w+', ShowPage)
        ],
        debug=True)

def main(): run_wsgi_app(application)

if __name__ == "__main__": main()
