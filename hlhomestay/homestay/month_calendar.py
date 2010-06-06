# coding=utf8
from lib import template 
from lib.template import Template 
from calendar import Calendar
from datetime import date
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app

temp = '''$def with (m)
<table id="canlendar">
<tr id="week_label">
<td>一</td><td>二</td><td>三</td><td>四</td>
<td>五</td><td>六</td><td>日</td>
</tr>
$for w in m:
    <tr class="week">
    $for d in w:
        <td class="$:date_class(d)">$:d.day</td>
    </tr>
</table>
'''

def date_class(d):
    if d == date.today():
        return 'today'
    elif d < date.today():
        return 'passed'
    elif d > date.today():
        return 'future'
    
temp = Template(temp, globals = {"date_class":date_class})


class MainPage(webapp.RequestHandler):
  def get(self):
      c = Calendar()
      m = c.monthdatescalendar(2010, 5)

      render = template.frender('month_calendar.html', 
                                 globals = {"date_class":date_class})

      self.response.headers['Content-Type'] = 'text/html'
      self.response.out.write(str(render(m)))

application = webapp.WSGIApplication(
                                     [('/', MainPage)],
                                     debug=True)

def main():
    run_wsgi_app(application)

if __name__ == "__main__":
  main()
