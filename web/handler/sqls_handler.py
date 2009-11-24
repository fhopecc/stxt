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
        print "<<<<<<+ " + req.GET['id']
        #print "<<<<<< " + id.encode('cp950')

    conn = sqlite3.connect('d:/stxt/test.db')
    ts = conn.cursor()
    tables = ts.execute('select name from sqlite_master')

    zt = Template(filename="template\zh_tw.html", escape=pyratemp.HTML)
    t = Template(filename="template\sqls.html", escape=pyratemp.HTML)
    tab_sel = Template(filename=r"template\table_selector.html", escape=pyratemp.HTML) 

    ts_res = tab_sel(rows=tables)

    status = '200 OK'
    response_headers = [('Content-type','text/html')]
    start_response(status, response_headers)
    return [zt(body=t(left_menu=ts_res, result=r,id=id)).encode("utf-8")]

def show(environ, start_response):
    req = Request(environ)

    id, r = '', ''
    if req.GET.has_key('id'):
        id = req.GET['id'].decode('utf8')
        print "<<<<<<+ " + req.GET['id']
        print "<<<<<<" + id

    conn = sqlite3.connect('d:/stxt/test.db')
    c = conn.cursor()
    rs = c.execute(id)

    ts = conn.cursor()
    tables = ts.execute('select name from sqlite_master')

    zt = Template(filename="template\zh_tw.html", escape=pyratemp.HTML)
    t = Template(filename="template\sqls.html", escape=pyratemp.HTML)
    tab = Template(filename=r"template\table.html", escape=pyratemp.HTML) 
    tab_sel = Template(filename=r"template\table_selector.html", escape=pyratemp.HTML) 

    ts_res = tab_sel(rows=tables)

    status = '200 OK'
    response_headers = [('Content-type','text/html')]
    start_response(status, response_headers)
    return [zt(body=t(left_menu=ts_res, result=tab(rows=rs), id=id)).encode("utf-8")]

def destroy(request, response):
    return "del test %d" % id
