# coding=utf8
import sqlite3
from webob import Request
from lib.pyratemp import Template
from lib import pyratemp
def index(environ, start_response):
    req = Request(environ)

    zt = Template(filename="template\zh_tw.html", escape=pyratemp.HTML)
    t = Template(filename="template\index.html", escape=pyratemp.HTML)

    status = '200 OK'
    response_headers = [('Content-type','text/html')]
    start_response(status, response_headers)
    return [zt(body=t()).encode("utf-8")]
