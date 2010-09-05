import sys, os, cgi, re
from datetime import date
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from lib import template 
from lib.template import Template 
import logging
from model import Homestay

def get_account(path):
    pat = r'/homestays/(\w+)'
    m = re.match(pat, path)
    return m.group(1)

class AdminPage(webapp.RequestHandler):
    def homestay(self):
        p = r'/admin/(\w+)'
        m = re.match(p, self.request.path)
        k = m.group(1)
        h = Homestay.get(k)
        return h

    def room(self):
        p = r'/admin/(\w+)/\d+'                  # pattern
        m = re.match(p, self.request.path) # match
        k = m.group(1)                     # key for homestay
        r = Room.get(k)                    # Room object
        return r

    def date(self):
        p = r'/admin/\w+/(\d+)' 
        m = re.match(p, self.request.path)
        d = m.group(1)     
        d = strpdate(d, '%Y%m%d')
        return d

    def month(self):
        p = r'/admin/\w+(/(\d{6}))?'
        m = re.match(p, self.request.path)

        if not m.group(2): return date.today()

        d = m.group(2) + '01'     
        d = strpdate(d)
        return d

class IndexPage(AdminPage):
    def get(self):
        homestay = self.homestay()
        month = self.month()
        available_rooms_in_month = homestay.available_rooms_in_month(month.year, month.month)

        template_values = {
            'h': homestay,
            'today':date.today(),
            'month':month,
            'last_month':last_month(month),
            'next_month':next_month(month),
            'available_rooms_in_month':available_rooms_in_month
        }

        self.response.out.write(
            template.render('index.html', template_values))



application = webapp.WSGIApplication([('/admin/\w+', IndexPage)
                                     ],
                                     debug=True)

def main():
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
