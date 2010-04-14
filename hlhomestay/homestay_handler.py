import sys, os, cgi
#sys.path.insert(0, os.path.abspath(
#  os.path.join(os.path.dirname(__file__), '..', 'config')))
#import env, homestay, rest 
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.ext.webapp import template
from homestay import *
from datetime import date

class MainPage(webapp.RequestHandler):
  def get(self):
    self.response.out.write('Hello, ia ma  ')
    hs = Homestay.all()
    for h in hs:
      self.response.out.write(h.name)
      
  def post(self):
    h = Homestay(name = self.request.get('name'), 
                 link = self.request.get('link'), 
                 email = self.request.get('email'), 
                 status = 'invalid', 
                 create_at = date.today())
    h.put()
    self.response.out.write('Hello ' + h.name)
    hs = db.GqlQuery("SELECT * FROM Homestay ORDER BY date DESC LIMIT 10")
    for h in hs:
      self.response.out.write('Hello ' + h.name)

class NewPage(webapp.RequestHandler):
  def get(self):
    template_values = {
      'layout': os.path.dirname(__file__) + '/views/layout/homestay.html', 
      'greetings': ['a','2','e']
      }
    self.response.out.write(
        template.render('views/homestay/new.html',
      template_values))

application = webapp.WSGIApplication(
                                     [('/homestay', MainPage),
                                      ('/homestay/new', NewPage)
                                     ],
                                     debug=True)

def main():
  run_wsgi_app(application)

if __name__ == "__main__":
  main()
