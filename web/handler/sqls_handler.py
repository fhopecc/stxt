# coding=utf8
import sqlite3
from webob import Request
from lib.pyratemp import Template
from lib import pyratemp
def index(environ, start_response):
    req = Request(environ)
    id, r = '', ''
    if req.GET.has_key('id'):
        id = req.GET['id'].decode('utf8')

    conn = sqlite3.connect('d:/stxt/test.db')
    ts = conn.cursor()
    tables = ts.execute('select name from sqlite_master')

    t = Template(filename=r"template\sqls.html", escape=pyratemp.HTML) 

    def result(): return ''

    status = '200 OK'
    response_headers = [('Content-type','text/html')]
    start_response(status, response_headers)
    return [t(tables=tables, result=result, id=id).encode("utf-8")]

def show(environ, start_response):
    req = Request(environ)
#session = environ['paste.session.factory']()
#    user = 'unknown'
#    if session.has_key('user'):
#        user = session['user']
#    print 'user %s is loggon' % user

    id, r = '', ''
    if req.GET.has_key('id'):
        id = req.GET['id'].decode('utf8')

    conn = sqlite3.connect('d:/stxt/test.db')
    c = conn.cursor()
    rs = c.execute(id)

    ts = conn.cursor()
    tables = ts.execute('select name from sqlite_master')

    t = Template(filename=r"template\table.html", escape=pyratemp.HTML) 

    status = '200 OK'
    response_headers = [('Content-type','text/html')]
    start_response(status, response_headers)
    return [t(tables=tables, rows=rs, id=id).encode("utf-8")]

def destroy(request, response):
    return "del test %d" % id
