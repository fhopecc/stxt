import sys, os, cgi, re
from datetime import date
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template
from google.appengine.ext.webapp.util import run_wsgi_app
from model import *

class SysPage(webapp.RequestHandler):
    def homestay(self):
        p = r'/sys/(\w+)(/.*)?' # pattern
        m = re.match(p, self.request.path)    # match
        k = m.group(1)                # key for homestay
        h = Homestay.get(k)           # Homestay object
        return h

class IndexPage(SysPage):
    def get(self):
        if not users.is_current_user_admin():
            self.redirect(users.create_login_url('/sys'))
            return

        hs = Homestay.all()
        template_values = {
            'homestays': hs
        }
        self.response.out.write(
             template.render('index.html', template_values))

class NewPage(SysPage):
    def get(self):
        template_values = {}
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
        self.redirect('/sys')

class ShowPage(SysPage):
    def get(self):
        homestay = self.homestay()
        template_values = {
            'h': homestay
        }
        self.response.out.write(
             template.render('show.html', template_values))

class EditPage(SysPage):
    def get(self):

        template_values = {
            'h': self.homestay()
        }

        self.response.out.write(
             template.render('edit.html', template_values))

    # update entity
    def post(self):
        r = self.request
        h = Homestay.get(r.get("key"))
        h.owner = users.User(r.get("owner"))
        h.name = r.get("name")
        h.css = r.get("css")
        h.email = r.get("email")
        h.blog = r.get("blog")
        h.phone = r.get("phone")
        h.mobile = r.get("mobile")
        h.address = r.get("address")
        h.comment = r.get("comment")
        h.put()
        self.redirect('/sys/%s' % h.key())

application = webapp.WSGIApplication([
                ('/sys', IndexPage),
                ('/sys/new', NewPage), 
                ('/sys/\w+/edit', EditPage), 
                ('/sys/\w+', ShowPage)
               ], debug=True)

def main():
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
