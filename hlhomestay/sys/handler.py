import sys, os, cgi, re
from datetime import date
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template
from google.appengine.ext.webapp.util import run_wsgi_app
from model import *

class SysPage(webapp.RequestHandler):
    pass

class IndexPage(SysPage):
    def get(self):
        hs = Homestay.all()
        template_values = {
            'homestays': hs,
        }
        self.response.out.write(
             template.render('index.html', template_values))

class NewPage(SysPage):
    def get(self):
        self.response.out.write(
             template.render('new.html', template_values))

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

application = webapp.WSGIApplication([
                ('/sys', IndexPage),
                ('/sys/new', NewPage)
               ], debug=True)

def main():
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
