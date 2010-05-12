# coding=utf-8
from __future__ import with_statement
import sys, os, re, unittest, slide
from slide import *

logging.basicConfig(level=logging.DEBUG, filename='stxt.log',
                    filemode='w')
logger = logging.getLogger('stxt.formatter.slide_print')

def to_slide(tree):
    html = ""
    for sect1 in tree.children:
       html += str(disp(sect1))
    return html

def f_sect1(tree):
    temp = '''$def with (id, type, title, content)
<div id="$id" class="$type"><div class="title">$title</div>
$:content
</div>
'''
    content =""
    cs = [c for c in tree.children if c.type != 'sect2']
    for c in cs: content += disp(c)

    temp = Template(temp)
    content =  str(temp(f_address(tree), tree.type, f_title(tree), content))

    sect2s = [sect2 for sect2 in tree.children if sect2.type == 'sect2']
    for sect2 in sect2s:
       content += disp(sect2)
    return content

slide.f_sect1 = f_sect1

def f_sect2(tree):
    temp = '''$def with (id, type, title, subtitle, content)
<div id="$id" class="$type"><div class="title">$title
<div class="subtitle">$subtitle</div>
</div>
$:content
</div>
'''
    content =""
    for c in tree.children: content += disp(c)

    temp = Template(temp)

    content = str(temp(f_address(tree), tree.type, 
                f_title(tree), "", content))

    return str(render.slide(f_title(tree), content))

slide.f_sect2 = f_sect2
