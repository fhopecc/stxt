# coding=utf-8
from __future__ import with_statement
import sys, os, re, unittest, stxt_parser, slide
from slide import *

logging.basicConfig(level=logging.DEBUG,
                    filename='stxt.log',
                    filemode='w')
logger = logging.getLogger('stxt.formatter.slide_print')

def to_slide(tree):
    for sect1 in tree.children:
        disp(sect1)

def f_sect1(tree):
    temp = '''$def with (id, type, title, content)
<div id="$id" class="$type"><div class="title">$title</div>
$:content
</div>
<div id="footer">
<input id="prev" type="hidden" value="0.html"/>
<input id="next" type="hidden" value="1.html"/>
</div>
'''
    content =""
    cs = [c for c in tree.children if c.type != 'sect2']
    for c in cs: content += disp(c)

    temp = Template(temp)
    content =  str(temp(f_address(tree), tree.type, f_title(tree), content))

    dir = path.dirname(f_path(tree))
    if not path.exists(dir):
        os.makedirs(dir) 

    with open(f_path(tree), 'w') as f:
        f.write(str(render.slide(f_title(tree), content)))

    logger.info('render %s' % f_path(tree))

    sect2s = [sect2 for sect2 in tree.children if sect2.type == 'sect2']
    for sect2 in sect2s:
        disp(sect2)

def f_sect2(tree):
    temp = '''$def with (id, type, title, subtitle, content, prev, next)
<div id="$id" class="$type"><div class="title">$title
<div class="subtitle">$subtitle</div>
</div>
$:content
</div>
<div id="footer">
<input id="prev" type="hidden" value="$prev"/>
<input id="next" type="hidden" value="$next"/>
</div>
'''
    content =""
    for c in tree.children: content += disp(c)

    sect2s = [c for c in tree.sibling() if c.type == 'sect2']
    order = sect2s.index(tree)
    prev = order-1
    next = order+1
    if prev > -1:
        prev = sect2s[prev]
    if type(prev) is type(tree):
        prev = f_filename(prev)
    else:
        prev = f_filename(tree.parent)

    if next < len(sect2s):
        next = sect2s[next]
    if type(next) is type(tree):
        next = f_filename(next)
    else:
        next = f_filename(tree)

    subtitle = '%s/%s' % (order + 1, len(sect2s))

    temp = Template(temp)
    content = str(temp(f_address(tree), tree.type, 
                f_title(tree), subtitle, content, prev, next))

    dir = path.dirname(f_path(tree))
    if not path.exists(dir):
        os.mkdirs(dir) 

    with open(f_path(tree), 'w') as f:
        f.write(str(render.slide(f_title(tree), content)))

    logger.info('render %s' % f_path(tree))

