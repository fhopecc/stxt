import sys, os, cgi, re
from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.ext.webapp import template
from datetime import date
from model import *

class HomestayPage(webapp.RequestHandler):
    def homestay(self):
        user = users.get_current_user()
        homestay = Homestay.all().filter('owner', user).get()
        return homestay

    def room(self):
        p = r'/admin/(\w+)/\d{8}'   
        m = re.match(p, self.request.path)
        r = m.group(1)                   
        r = Room.get(r)                 
        return r

    def reservation(self):
        p = r'/admin/(\w+)'            # pattern
        m = re.match(p, self.request.path) # match
        k = m.group(1)                     # key for homestay
        r = Reservation.get(k)                    # Room object
        return r

    def logout_url(self):
        return users.create_logout_url('/admin')

    def date(self):
        p = r'/admin/\w+/(\d{8})' 
        m = re.match(p, self.request.path)
        if not m: return date.today()
        d = m.group(1)     
        d = strpdate(d, '%Y%m%d')
        return d

class EditHomestayPage(HomestayPage):
    def get(self):
        h = self.homestay()
        template_values = {
            'h': h
        }
        self.response.out.write(
            template.render('edithomestay.html', template_values))

    def post(self):
        r = self.request
        h = self.homestay()

        h.name = r.get("name")
        h.address = r.get("address")
        h.email = r.get("email")
        h.phone = db.PhoneNumber(r.get("phone"))
        h.blog = r.get("blog")
        h.css = r.get("css")
        h.notice = r.get("notice")
        h.put()
        self.redirect('/admin')


class IndexPage(HomestayPage):
    def month(self):
        p = r'/admin/(\d{6})'
        m = re.match(p, self.request.path)

        if not m: return date.today()
        d = m.group(1) + '01'     
        d = strpdate(d)
        return d

    def last_month_path(self):
        year = self.month().year
        month = self.month().month
        last_month = month - 1
        if last_month < 1:
            last_month = 12
            year -= 1
        last_month = date(year, last_month, 1) 

        h = self.homestay()

        return '/admin/%s' % last_month.strftime('%Y%m')

    def next_month_path(self):
        year = self.month().year
        month = self.month().month
        next_month = month + 1
        if next_month > 12:
            next_month = 1
            year += 1
        next_month = date(year, next_month, 1)

        h = self.homestay()

        return '/admin/%s' % next_month.strftime('%Y%m')

    def get(self):
        h = self.homestay()

        if not h:
            self.redirect(users.create_logout_url('/admin'))
            return
        
        template_values = {
            'h': h,
            'today':date.today(),
            'month':self.month(),
            'last_month_path':self.last_month_path(),
            'next_month_path':self.next_month_path(),
            'monthly_books': h.monthly_books(
                                    self.month().year, 
                                    self.month().month), 
            'logout_url':self.logout_url()
        }
        self.response.out.write( template.render('index.html', template_values))

# NewPage for booking a room
class NewPage(HomestayPage):
    def get(self):
        r = self.room()
        d = self.date()

        from datetime import timedelta
        res = Reservation(room = r, 
                          checkin = d, 
                          checkout = d + timedelta(days=1)
                          )
        
        template_values = {
            'h': res.room.homestay,
            'res': res
        }

        self.response.out.write(template.render('new.html', 
                                template_values))

    def post(self):
        r = self.request
        res = Reservation(name = r.get('name') ,
                          phone = db.PhoneNumber(r.get('phone')), 
                          email = r.get('email'), 
                          checkin = strpdate(r.get('checkin')), 
                          checkout = strpdate(r.get('checkout')), 
                          addbeds_num = int(r.get('addbeds_num')), 
                          create_date = strpdate(r.get('create_date')),
                          comment = r.get('comment'), 
                          room = self.room())
        try:
            res.book()

            template_values = {
                'h': res.room.homestay, 
                'res': res
            }

            self.response.out.write(template.render('show.html', 
                                    template_values))

        except PeriodHasBooksError:
            template_values = {
                'h': res.room.homestay, 
                'bs': res.period_books()
            }
            self.response.out.write(
                    template.render('period_has_books_error.html', 
                                    template_values))

class ShowPage(HomestayPage):
   def get(self):
        res = self.reservation()

        template_values = {
            'h': res.room.homestay, 
            'res': res
        }

        self.response.out.write(template.render('show.html', 
                                template_values))

class EditPage(HomestayPage):
    def get(self):
        res = self.reservation()

        template_values = {
            'h': res.room.homestay, 
            'res': res
        }
        self.response.out.write(
            template.render('edit.html', template_values))

    def post(self):
        r = self.request

        res = Reservation.get(r.get('key'))

        res.name  = r.get('name')
        res.phone = db.PhoneNumber(r.get('phone'))
        res.email = r.get('email') 
        res.checkin = strpdate(r.get('checkin'))
        res.checkout = strpdate(r.get('checkout')) 
        res.addbeds_num = int(r.get('addbeds_num'))
        res.create_date = strpdate(r.get('create_date'))
        res.comment = r.get('comment') 
        res.room = Room.get(r.get('room'))
        res.put()

        template_values = {
            'h': res.room.homestay, 
            'res': res
        }

        self.response.out.write(template.render('show.html', 
                                template_values))

class DelPage(HomestayPage):
    def get(self):
        res = self.reservation()
        res.delete()
        self.redirect('/admin')

class AdminRoomPage(HomestayPage):
    def room(self):
        p = r'/admin/room/(\w+)/\w+'       # pattern
        m = re.match(p, self.request.path) # match
        k = m.group(1)                     # key for homestay
        r = Room.get(k)                    # Room object
        return r

class NewRoomPage(AdminRoomPage):
    def get(self):
        homestay = self.homestay()
        room = Room(homestay=homestay)
        template_values = {
            'h': homestay, 
            'r': room
        }
        self.response.out.write(
            template.render('newroom.html', template_values))

    def post(self):
        r = self.request
        room = Room(name = r.get("name") ,
                    homestay = self.homestay())
        room.put()
        self.redirect('/admin')

class EditRoomPage(AdminRoomPage):
    def get(self):
        room = self.room()
        template_values = {
            'h': room.homestay, 
            'r': room
        }
        self.response.out.write(
            template.render('editroom.html', template_values))

    def post(self):
        r = self.request
        room = self.room()
        room.name = r.get("name")
        room.put()
        self.redirect('/admin')

class DelRoomPage(AdminRoomPage):
    def get(self):
        room = self.room()
        room.delete()
        self.redirect('/admin')

class NewPriceTypePage(HomestayPage):
    def get(self):
        r = self.request
        room = Room.get(r.get("room"))
        price_type = PriceType(room=room)

        template_values = {
            'h': room.homestay, 
            'r': room,
            'p': price_type
        }

        self.response.out.write(
            template.render('new_price_type.html', template_values))

    def post(self):
        r = self.request
        room = Room.get(r.get("room"))
        price_type = PriceType(room=room,
                        name=r.get("name"),
                        price=int(r.get("price")),
                        holiday_price=int(r.get("holiday_price")),
                        bed_price=int(r.get("bed_price")),
                     )
        price_type.put()

        self.redirect(room.edit_path())

class EditPriceTypePage(HomestayPage):
    def price_type(self):
        p = r'/admin/price_types/(\w+)/edit' # pattern
        m = re.match(p, self.request.path)   # match
        if m:
            k = m.group(1)                   # key for homestay
            p = PriceType.get(k)             # Room object
        else:
            p = PriceType.get(self.request.get('price_type'))
        return p

    def get(self):
        p = self.price_type()

        template_values = {
            'h': p.room.homestay,
            'r': p.room,
            'p': p
        }

        self.response.out.write(
            template.render('edit_price_type.html', template_values))

    def post(self):
        r = self.request
        p = self.price_type()

        p.name          = r.get("name")
        p.price         = int(r.get("price"))
        p.holiday_price = int(r.get("holiday_price"))
        p.bed_price     = int(r.get("bed_price"))
         
        p.put()

        self.redirect("/admin")


class HolidaysPage(HomestayPage):

    def month(self):
        p = r'/admin/holidays/(\d{6})'
        m = re.match(p, self.request.path)

        if not m: return date.today()
        d = m.group(1) + '01'     
        d = strpdate(d)
        return d

    def last_month_path(self):
        year = self.month().year
        month = self.month().month
        last_month = month - 1
        if last_month < 1:
            last_month = 12
            year -= 1
        last_month = date(year, last_month, 1) 

        h = self.homestay()

        return '/admin/holidays/%s' % last_month.strftime('%Y%m')

    def next_month_path(self):
        year = self.month().year
        month = self.month().month
        next_month = month + 1
        if next_month > 12:
            next_month = 1
            year += 1
        next_month = date(year, next_month, 1)

        h = self.homestay()

        return '/admin/holidays/%s' % next_month.strftime('%Y%m')

    def get(self):
        h = self.homestay()

        template_values = {
            'h': h,
            'today':date.today(),
            'month':self.month(),
            'last_month_path':self.last_month_path(),
            'next_month_path':self.next_month_path(),
            'monthly_holidays': 
                h.monthly_holidays(self.month().year, 
                                   self.month().month)
        }
        self.response.out.write(template.render('holidays.html', 
                                template_values))

class NewHolidayPage(HolidaysPage):

    def date(self):
        p = r'/admin/holidays/(\d{8})/new' 
        m = re.match(p, self.request.path)
        if not m: return date.today()
        d = m.group(1)     
        d = strpdate(d, '%Y%m%d')
        return d

    def get(self):
        h = self.homestay()
        d = self.date()

        holiday = Holiday(homestay = h, 
                          date = d
                          )
        
        template_values = {
            'h': h,
            'holiday': holiday
        }

        self.response.out.write(template.render('new_holiday.html', 
                                template_values))

    def post(self):
        r = self.request
        h = Holiday(date = strpdate(r.get('date')), 
                    name = r.get('name'),
                    isholiday = r.get('name') != 'False',
                    homestay = self.homestay())
        h.put()

        self.redirect('/admin/holidays/%s' % h.date.strftime('%Y%m'))

class EditHolidayPage(HolidaysPage):
    def holiday(self):
        p = r'/admin/holidays/(\w+)/edit'  
        m = re.match(p, self.request.path) 
        k = m.group(1)                     
        return  Holiday.get(k)             

    def get(self):
        holiday = self.holiday()

        template_values = {
            'h': holiday.homestay, 
            'holiday': holiday
        }
        self.response.out.write(
            template.render('edit_holiday.html', template_values))

    def post(self):
        r = self.request
        holiday = self.holiday()
        holiday.name = r.get("name")
        holiday.date = strpdate(r.get('date'))
        holiday.isholiday = r.get('name') != 'False'
        holiday.put()

        self.redirect(holiday.calendar_path())

class DelHolidayPage(HolidaysPage):
    def holiday(self):
        p = r'/admin/holidays/(\w+)/delete'
        m = re.match(p, self.request.path) 
        k = m.group(1)                     
        return  Holiday.get(k)             

    def get(self):
        holiday = self.holiday()
        holiday.delete()
        self.redirect(holiday.calendar_path())

class SpecialsPage(HomestayPage):

    def month(self):
        p = r'/admin/specials/(\d{6})'
        m = re.match(p, self.request.path)

        if not m: return date.today()
        d = m.group(1) + '01'     
        d = strpdate(d)
        return d

    def last_month_path(self):
        year = self.month().year
        month = self.month().month
        last_month = month - 1
        if last_month < 1:
            last_month = 12
            year -= 1
        last_month = date(year, last_month, 1) 

        h = self.homestay()

        return '/admin/specials/%s' % last_month.strftime('%Y%m')

    def next_month_path(self):
        year = self.month().year
        month = self.month().month
        next_month = month + 1
        if next_month > 12:
            next_month = 1
            year += 1
        next_month = date(year, next_month, 1)

        h = self.homestay()

        return '/admin/specials/%s' % next_month.strftime('%Y%m')

    def get(self):
        h = self.homestay()

        template_values = {
            'h': h,
            'today':date.today(),
            'month':self.month(),
            'last_month_path':self.last_month_path(),
            'next_month_path':self.next_month_path(),
            'monthly_specials': 
                h.monthly_specials(self.month().year, 
                                   self.month().month)
        }
        self.response.out.write(template.render('specials.html', 
                                template_values))

class NewSpecialPage(HomestayPage):

    def room(self):
        p = r'/admin/specials/(\w+)/\d{8}'   
        m = re.match(p, self.request.path)
        r = m.group(1)                   
        r = Room.get(r)                 
        return r

    def date(self):
        p = r'/admin/specials/\w+/(\d{8})' 
        m = re.match(p, self.request.path)
        if not m: return date.today()
        d = m.group(1)     
        d = strpdate(d, '%Y%m%d')
        return d

    def get(self):
        s = Special(room = self.room(), 
                    date = self.date()
                   )
        
        template_values = {
            'h': s.room.homestay,
            'special': s
        }

        self.response.out.write(template.render('new_special.html', 
                                template_values))

    def post(self):
        r = self.request
        s = Special(room = Room.get(r.get('room')),
                    date = strpdate(r.get('date')),
                    name = r.get('name'),
                    price = int(r.get('price'))
                   )
        s.put()

        self.redirect(s.calendar_path())

class EditSpecialPage(SpecialsPage):
    def special(self):
        p = r'/admin/specials/(\w+)/edit'  
        m = re.match(p, self.request.path) 
        k = m.group(1)                     
        return  Special.get(k)             

    def get(self):
        special = self.special()

        template_values = {
            'h': special.room.homestay, 
            'special': special
        }

        self.response.out.write(
            template.render('edit_special.html', template_values))

    def post(self):
        r = self.request
        special = Special.get(r.get('special'))
        #special.room = Room.get(r.get('room'))
        #special.date = strpdate(r.get('date'))
        special.name = r.get("name")
        special.price = int(r.get('price'))
        special.put()

        self.redirect(special.calendar_path())

class DelSpecialPage(SpecialsPage):
    def special(self):
        p = r'/admin/specials/(\w+)/delete'  
        m = re.match(p, self.request.path) 
        k = m.group(1)                     
        return  Special.get(k)             

    def get(self):
        special = self.special()
        special.delete()
        self.redirect(special.calendar_path())

class PeriodBooksPage(HomestayPage):
    def get(self):
        r = self.request
        room = Room.get(r.get('room'))

        bs = room.period_books(strpdate(r.get('checkin')),
                               strpdate(r.get('checkout')))

        o = ','.join(['{key:"%s", name:"%s", checkin:%s, checkout:%s}' % 
                       (b.key(),
                        b.name, 
                        date2json(b.checkin), 
                        date2json(b.checkout)
                       ) for b in bs])

        self.response.out.write('[%s]' % o)

def date2json(d):
    'month value whose range starts with zero, so Nov = 10, Dec = 11'
    return 'new Date(%d, %d, %d)' % (d.year, d.month - 1, d.day)
        
application = webapp.WSGIApplication([
               (r'/admin', IndexPage), 
               (r'/admin/\d{6}', IndexPage),
               (r'/admin/period_books', PeriodBooksPage), 
               (r'/admin/homestay/edit', EditHomestayPage), 
               (r'/admin/holidays', HolidaysPage), 
               (r'/admin/holidays/\d{6}', HolidaysPage), 
               (r'/admin/holidays/new', NewHolidayPage), 
               (r'/admin/holidays/\d{8}/new', NewHolidayPage), 
               (r'/admin/holidays/\w+/edit', EditHolidayPage), 
               (r'/admin/holidays/\w+/delete', DelHolidayPage), 
               (r'/admin/specials', SpecialsPage), 
               (r'/admin/specials/\d{6}', SpecialsPage), 
               (r'/admin/specials/new', NewSpecialPage), 
               (r'/admin/specials/\w+/\d{8}', NewSpecialPage),
               (r'/admin/special/edit', EditSpecialPage), 
               (r'/admin/specials/\w+/edit', EditSpecialPage), 
               (r'/admin/specials/\w+/delete', DelSpecialPage), 
               (r'/admin/room/new', NewRoomPage), 
               (r'/admin/room/\w+/edit', EditRoomPage), 
               (r'/admin/room/\w+/delete', DelRoomPage), 
               (r'/admin/price_types/new.*', NewPriceTypePage), 
               (r'/admin/price_types/\w+/edit', EditPriceTypePage), 
               (r'/admin/price_types/edit', EditPriceTypePage), 
               (r'/admin/edit', EditPage),
               (r'/admin/\w+', ShowPage),
               (r'/admin/\w+/edit', EditPage),
               (r'/admin/\w+/delete', DelPage), 
               (r'/admin/\w+/\d{8}', NewPage)
              ], debug=True)

def main():
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
