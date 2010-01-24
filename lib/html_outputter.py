# coding=utf-8
# for single html
from __future__ import with_statement
from template import Template
#from pygments import highlight
#from pygments.lexers import PythonLexer
#from pygments.formatters import HtmlFormatter
import sys, os, re, booker, template

def disp(tree):
    if re.match(r'sect\d', tree.type):
        return f_sect(tree)
    elif tree.type in ('question', 'answer', 'define', 'theorem'):
        return f_label_container(tree)
    elif tree.type in ('proof'):
        return f_label_container(tree)
    elif tree.type in ('para'):
        return f_element(tree)
    elif tree.type in ('list', 'listitem', 'olistitem'):
        return f_container(tree)
    else: return globals()['f_' + tree.type](tree)

def to_html(file):
    d = None
    with open(file) as f:
        d = booker.parse(f.read())
    d.number_children()
    d.count_occurence()
    title = os.path.basename(file)

    render = template.render('template')
    return str(render.single_html(title, disp(d)))

def f_doc(tree):
    html = ''
    for e in tree.children:
        html += disp(e)
    return html

def f_sect(tree):
    html = '<div class="title">%s%s</div>\n' % \
            (f_section_number(tree), tree.value)
    for c in tree.children:
        html += disp(c)
    html = '<div class="%s">\n%s\n</div>' % (tree.type, html)
    return html

def f_term(tree):
    html = '<div class="title">%s</div>\n' % tree.value
    for c in tree.children: html += disp(c)
    html = '<div class="%s">\n%s\n</div>' % (tree.type, html)
    return html

def f_type_label(tree):
    return {'define'  : '定義', 
            'theorem' : '定理', 
            'proof'   : '證明', 
            'question': '題', 
            'answer'  : '答', 
            'image'   : '圖' 
           }[tree.type]

def f_label_container(tree):
    temp = '''$def with (type, label, title, content)
<div class="$type"><div class="title">$label：$title</div>
$content
</div>
'''
    label = f_type_label(tree) + str(tree.order() + 1)
    content =""
    for c in tree.children: content += disp(c)
    temp = Template(temp)
    return str(temp(tree.type, label, tree.title, content))

def f_image(tree):
    temp = '''$def with (type, label, title, path)
<div class="$type">
    <div class="title">$label：$title</div>
    <img src="images/$path" alt="$title"/>
</div>
'''
    label = f_type_label(tree) + str(tree.order() + 1)
    title = tree.value
    temp = Template(temp)
    return str(temp(tree.type, label, tree.title, tree.name))

def f_table(tree):
    html = '<h4>表%s：%s</h4>\n'%(tree.occurence,  tree.title)
    if tree.children:
        html += '<table>\n'
        for row in tree.children:
            html += '<tr>\n' 
            for col in row.children:
                html += '<td>%s</td>\n' % col.value.encode('utf8')
            html += '</tr>\n' 
        html += '</table>\n'
    else:
        html += '<pre>%s</pre>\n' % tree.value 
    return html

def f_answer(tree):
    html = '<h4>答：</h4>\n'
    for c in tree.children:
        html += disp(c)
    return html

def f_sect2(tree):
    html = '<h2>%s%s</h2>\n'%(f_section_number(tree), tree.title)
    for c in tree.children:
        html += disp(c)
    return html

def f_sect3(tree):
    html =    '<h3>%s</h3>\n'% tree.title
    for c in tree.children:
        html += disp(c)
    return html

def f_code(tree):
    html    = '<h4>程式碼%s：%s</h4>\n'%(tree.occurence,    tree.title)
    #html += tree.value + '</pre>\n'
    html += '<pre>%s</pre>\n' % tree.value
    #highlight(tree.value.decode('utf8'), PythonLexer(), HtmlFormatter())
    #print highlight(tree.value, PythonLexer(), HtmlFormatter())
    return html

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

def f_filename(tree):
    fn = tree.section_number()[0]
    if tree.name:
        fn = tree.name
    return '%s.html' % fn

if __name__ == '__main__':
    usage = os.path.basename(__file__) + " filename"
    try:
        print to_html(sys.argv[1])
    except IndexError:
        print usage
