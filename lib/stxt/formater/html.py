# coding=utf-8
from __future__ import with_statement
import sys, os, re
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from stxt import parser
from stxt import template 
from stxt.template import Template 
from stxt.parser import console

def disp(tree):
    if re.match(r'sect\d', tree.type):
        return f_titled_container(tree)
    elif tree.type in ('question', 'answer', 'define', 'theorem'):
        return f_titled_container(tree)
    elif tree.type in ('proof', 'term'):
        return f_titled_container(tree)
    elif tree.type in ('para'):
        return f_element(tree)
    elif tree.type in ('list', 'listitem', 'olistitem'):
        return f_container(tree)
    else: return globals()['f_' + tree.type](tree)

def to_html(file):
    d = None
    d = parser.read(file)
    
    d.number_children()
    d.count_occurence()
    title = os.path.basename(file)

    f = open(os.path.join('structedtext', 'css', 'web.css'))
    css = f.read()
    f.close()

    render = template.render('template')
    return str(render.single_html(title, disp(d), css))

def f_doc(tree):
    html = ''
    for e in tree.children:
        html += disp(e)
    return html

def f_title(tree):
    title = ""
    if re.match(r'sect\d', tree.type):
        title = f_section_number(tree) + tree.title
    elif tree.type in ('question', 'define', 'theorem'):
        title =  f_label(tree) + str(tree.order() + 1)
        title += "：" + tree.title
    elif tree.type in ('proof', 'answer'):
        title =  f_label(tree) + '：'
    elif tree.type in ('table', 'image', 'code'):
        title =  f_label(tree) + str(tree.order() + 1)
        title += "：" + tree.title
    elif tree.type in ('term'):
        title = tree.title
    return title

def f_label(tree):
    return {'define'  : '定義', 
            'theorem' : '定理', 
            'proof'   : '證明', 
            'code'    : '程式', 
            'question': '題', 
            'answer'  : '答', 
            'table'   : '表', 
            'image'   : '圖' 
           }[tree.type]

def f_titled_container(tree):
    temp = '''$def with (type, title, content)
<div class="$type"><div class="title">$title</div>
$:content
</div>
'''
    content =""
    for c in tree.children: content += disp(c)
    temp = Template(temp)
    return str(temp(tree.type, f_title(tree), content))

def f_image(tree):
    temp = '''$def with (type, title, path)
<div class="$type"><div class="title">$title</div>
<img src="images/$path" alt="$title"/>
</div>
'''
    temp = Template(temp)
    return str(temp(tree.type, f_title(tree), tree.name))

def f_table(tree):
    temp = '''$def with (type, title, content)
<div class="$type"><div class="title">$title</div>
$:content
</div>
'''
    content = ''
    if tree.children:
        content += '<table>\n'
        for ri, row in enumerate(tree.children):
            content += '<tr>\n' 
            for col in row.children:
                if ri == 0:
                    content += '<th>%s</th>\n' % col.value.encode('utf8')
                else:
                    content += '<td>%s</td>\n' % col.value.encode('utf8')
            content += '</tr>\n' 
        content += '</table>\n'
    else:
        content += '<pre>%s</pre>\n' % tree.value 
    temp = Template(temp)
    return str(temp(tree.type, f_title(tree), content))

def f_code(tree):
    temp = '''$def with (type, title, code)
<div class="$type"><div class="title">$title</div>
<pre>
$:code
</pre>
</div>
'''
    temp = Template(temp)
    return str(temp(tree.type, f_title(tree), tree.value))

def f_container(tree):
    temp = '''$def with (type, element)
<div class="$type">
$element
</div>
'''
    element = ""
    for c in tree.children: element += disp(c)
    temp = Template(temp)
    return str(temp(tree.type, element))

def f_element(tree):
    temp = '''$def with (type, content)
<div class="$type">
$content
</div>
'''
    temp = Template(temp)
    return str(temp(tree.type, tree.value))

def f_list(tree):
    html = '<ul>\n'
    for c in tree.children:
        html += '<li>\n' + c.value 
        for np in c.children:
            html += disp(np)
        html += '</li>\n'
    html += '</ul>\n'
    return html

def f_olist(tree):
    temp = '''$def with (items)
<div class="olist">
$for i in items:
    <div class="olistitem">
    $ index = loop.index
    $for p in i.children:
        $if loop.first:
            <div class="para">
            $index.$(p.value)
            </div>
        $else:    
            $:(disp(p))
    </div>
</div>
'''
    globals = {'disp':disp}
    temp = Template(temp, globals=globals)
    return str(temp(tree.children))

def f_footnotes(tree):
    html = '<ul>\n'
    for c in tree.children:
        html += '<li>' + c.value+ '</li>\n'
    html += '</ul>\n'
    return html

def f_section_number(tree):
    level = tree.level
    n = '%s.' % (tree.order() + 1)
    for l in range(1, level):
        tree = tree.parent
        if not tree: break
        n = '%s.%s' % (tree.order() + 1, n)
    return n

def f_insert(tree):
    root = tree.root()
    insert = root.find_by_name(tree.node_name)
    return disp(insert)

def f_filename(tree):
    fn = tree.section_number()[0]
    if tree.name:
        fn = tree.name
    return '%s.html' % fn

def usage():
    usage = os.path.basename(__file__) + " filename\n"
    usage += u'filename: structed text file\n'
    usage += u'convert stxt to html.'
    return usage

if __name__ == '__main__':
    try:
        fn = sys.argv[1]
        print to_html(fn)
    except IndexError:
       console.info(usage()) 

