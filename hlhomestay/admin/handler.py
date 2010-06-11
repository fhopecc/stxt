import sys, os, cgi, re
from datetime import date
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from lib import template 
from lib.template import Template 
import logging

def get_account(path):
    pat = r'/homestays/(\w+)'
    m = re.match(pat, path)
    return m.group(1)

class IndexPage(webapp.RequestHandler):
  def get(self):
        render = template.frender('index.html')
        self.response.out.write(str(render()))

application = webapp.WSGIApplication(
                                     [('/admin/console', IndexPage)
                                     ],
                                     debug=True)

def main():
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
