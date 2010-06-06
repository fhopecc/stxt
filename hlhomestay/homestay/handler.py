import sys, os, cgi, re
from datetime import date
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from lib import template 
from lib.template import Template 
from homestay.model import Homestay
import logging

def get_account(path):
    pat = r'/homestays/(\w+)'
    m = re.match(pat, path)
    return m.group(1)

class MainPage(webapp.RequestHandler):
  def get(self):
        account = get_account(self.request.path)
        homestay = Homestay(account)
        render = template.frender('show.html')
        self.response.out.write(str(render(homestay)))
      
class NewPage(webapp.RequestHandler):
    def get(self):
        render = template.frender('new.html')

        self.response.headers['Content-Type'] = 'text/html'
        self.response.out.write(str(render()))

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
                                     [('/homestays/\w+', MainPage),
                                      ('/homestays/new', NewPage), 
                                     ],
                                     debug=True)

def main():
  run_wsgi_app(application)

if __name__ == "__main__":
  main()
