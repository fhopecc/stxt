# coding=utf-8
from __future__ import with_statement
import sys, os, re, html, web
from os import path
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

def f_slide_counts(tree):
    root = tree.root()
    sect2s = [c for c in tree.children() if c.type == 'sect2']
    return len(sect2s)
 
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

    console.info('render %s' % f_path(tree))

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
    if type(prev) is parser.Tree:
        prev = f_filename(prev)
    else:
        prev = f_filename(tree.parent)

    if next < len(sect2s):
        next = sect2s[next]
    if type(next) is parser.Tree:
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

def f_path(tree):
    'The file path for specified node'
    bf = path.basename(tree.root().file)
    dir = path.splitext(bf)[0]
    return path.join("slides", dir, f_filename(tree))

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
