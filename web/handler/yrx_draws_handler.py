# coding=utf8
from __future__ import with_statement
from task import yrx_draw_winning
def index(environ, start_response):
    t = ''
    with open(r'template\yrx_draws.html') as tfn:
        t = tfn.read()
        t = t.decode('utf8')

    output = t % {'title':u'網路申報抽獎程式', 'content':''}
    if output:
        start_response("200 OK", [('Content-Type','text/html')])
    return [output.encode('utf8')]

def show(id, environ, start_response):
    t = ''
    with open(r'template\yrx_draws.html') as tfn:
        t = tfn.read()
        t = t.decode('utf8')
    o = u'於時間 %s 抽出名單如下' %  id.replace('_', ':')
    agents = yrx_draw_winning.draw_agent_cases()
    human  = yrx_draw_winning.draw_normal_cases()
    o += u'<table><tr><th>一般民眾</th><th>專業人士</th></tr>'
    o += u'<tr><td><ol>'
    for c in human:
        o += u'<li>%s</li>' % c
    o += u'</ol></td>'
    o += '<td><ol>'
    for c in agents:
        o += u'<li>%s</li>' % c
    o += u'</ol></td></tr></table>'
    output = t % {'title':u'網路申報抽獎程式', 'content':o}
    if output:
        start_response("200 OK", [('Content-Type','text/html')])
    return [output.encode('utf8')]
