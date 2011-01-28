# coding=utf8
import sqlite3
def show(environ, start_response):
    id = environ['rest.id']
    
    conn = sqlite3.connect('d:/stxt/test.db')
    c = conn.cursor()
    emps = c.execute('select * from humans')
    print c.description
    out = ''
    for emp in emps:
        out += str(emp[1].encode('utf8'))

    status = '200 OK'
    response_headers = [('Content-type','text/plain')]
    start_response(status, response_headers)
    return ['Hello world! %s \n' % out]

def destroy(request, response):
    return "del test %d" % id
