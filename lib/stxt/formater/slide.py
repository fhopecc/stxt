# coding=utf-8
from __future__ import with_statement
import sys, os, re, html, web
from html import *
from web import f_path
from web import f_filename

render = template.render('template', globals={'css':f_css()})

def disp(tree):
    if re.match(r'sect[12]', tree.type):
        return globals()['f_' + tree.type](tree)
    else: 
        return html.disp(tree)

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

    with open(f_path(tree), 'w') as f:
        f.write(str(render.slide(f_title(tree), content)))

    console.info('render %s' % f_path(tree))

    sect2s = [sect2 for sect2 in tree.children if sect2.type == 'sect2']
    for sect2 in sect2s:
        disp(sect2)

def f_sect2(tree):
    temp = '''$def with (id, type, title, content, prev, next)
<div id="$id" class="$type"><div class="title">$title</div>
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
         if prev:
             prev = f_filename(prev)
         else:
             prev = f_filename(tree)

    if next < len(sect2s):
         next = sect2s[next]
         if next:
             next = f_filename(next)
         else:
             next = f_filename(tree)

    temp = Template(temp)
    content = str(temp(f_address(tree), tree.type, 
                f_title(tree), content, prev, next))

    with open(f_path(tree), 'w') as f:
        f.write(str(render.slide(f_title(tree), content)))

    console.info('render %s' % f_path(tree))

def f_comment(tree):
    return ''

def f_title(tree):
    return tree.title

html.f_title = f_title

def f_filename(tree):
    'The filename for specified node'
    if tree.type == 'sect1':
        return '0.html'
    else:
        return '%d.html' % (tree.order() + 1)

web.f_filename = f_filename

def usage():
    usage = os.path.basename(__file__) + " filename\n"
    usage += 'filename: structed text file'
    return usage

if __name__ == '__main__':
    if len(sys.argv) < 2: 
        console.info(usage())
        exit()
    sourcefile = sys.argv[1]
    tree = parser.read(sourcefile)
    tree.dump()
    to_slide(tree)
