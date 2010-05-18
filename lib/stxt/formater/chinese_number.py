# coding=utf-8
from __future__ import with_statement
import sys, os, re, unittest, booker

def disp(tree):
    return globals()['f_' + tree.type](tree)

def make_sect_list(doctree):
    o = '主題列表<br/>'
    for sect1 in doctree.children:
        o += r'<a href="%s.html">%s%s</a><br/>' %\
                 (f_section_number(sect1), f_section_number(sect1), sect1.title)
    return o

def to_html(file):
    d = None
    with open(file) as f:
         d = booker.parse(f.read())
    d.number_children()
    d.count_occurence()
    title = os.path.basename(file)
    with open(r'd:\stxt\template\chinese.html') as tfn:
        t = tfn.read()
        return t % {'title': title, 'content': disp(d)}

def f_section_number(tree):
    if tree.type == 'sect1': return ''
    #import pdb
    #pdb.set_trace()
    ns = tree.section_number()[-1] + 1
    cbd = ['零','壹','貳','參','肆','伍','陸','柒','捌','玖','拾',
           '拾壹','拾貳','拾參','拾肆','拾伍','陸','柒','捌','玖','拾']
    cd = ['零','一','二','三','四','五','六','七','八','九','十',
          '十一','十二','十三','十四','十五','十六','十七','十九','二十'
          '二十一','二十二','二十三','二十四','二十五','二十六',
          '二十七','二十八','二十九', '三十', 
          '三十一','三十二','三十三','三十四','三十五','三十六',
          '三十七','三十八','三十九'
         ]
    if tree.type == 'sect2':
        return cbd[ns] + '.'
    elif tree.type == 'sect3':
        return cd[ns] + '.'
    elif tree.type == 'sect4':
        return '(' + cd[ns] + ')' + ' '

def f_number(level, number):
    #import pdb
    #pdb.set_trace()
    number += 1
    digits = [['零','壹','貳','參','肆','伍','陸','柒','捌','玖','拾',
               '拾壹','拾貳','拾參','拾肆','拾伍','拾陸','拾柒','拾捌',
               '拾玖','貳拾'],
              ['零','一','二','三','四','五','六','七','八','九','十',
               '十一','十二','十三','十四','十五','十六','十七','十九'
               ,'二十','二十一','二十二','二十三','二十四','二十五',
               '二十六', '二十七','二十八','二十九', '三十', '三十一',
               '三十二','三十三','三十四','三十五','三十六', '三十七',
               '三十八','三十九']
              ] 
    if level == 0:
        return digits[0][number] + '.'
    if level == 1:
        return digits[1][number] + '.'
    elif level == 2:
        return '（%s）' % digits[1][number]
    elif level == 3:
        return str(number) + '.'
    elif level > 4:
        return '(%s)' % str(number)

def f_doc(tree):
    html = ''
    for sect1 in tree.children:
        html += disp(sect1)
    return html

def f_sect1(tree):
    html = '<div class="title">%s%s</div>\n' % \
            (f_section_number(tree), tree.value)
    for c in tree.children:
        html += disp(c)
    html = '<div class="%s">\n%s\n</div>' % (tree.type, html)
    return html

def f_sect2(tree):
    html = '<h2>%s%s</h2>\n' % (f_section_number(tree), tree.title)
    html += '<div class="sect2">'
    for c in tree.children:
        html += disp(c)
    html += '</div>'
    return html

def f_sect3(tree):
    html =  '<h3>%s%s</h3>\n' % (f_section_number(tree), tree.title)
    html += '<div class="sect3">'
    for c in tree.children:
        html += disp(c)
    html += '</div>'
    return html

def f_code(tree):
    html    = '<h4>表%s：%s</h4>\n'%(tree.occurence,    tree.title)
    html += '<pre>' + tree.value + '</pre>\n'
    #html += highlight(tree.value, PythonLexer(),
            #HtmlFormatter())
    #.decode('ascii').encode('utf8')
    #print highlight(tree.value, PythonLexer(), HtmlFormatter())
    return html

def f_table(tree):
    html    = '<h4>表%s：%s</h4>\n'%(tree.occurence, tree.title)
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

def f_para(tree):
    return '<p>\n' + tree.value + '</p>\n'

def f_l2para(tree):
    return '<p>\n' + tree.value + '</p>\n'

def f_list(tree):
    html = '<ul>\n'
    for c in tree.children:
        html += '<li>\n' + c.value 
        for np in c.children[1:-1]:
            html += disp(np)
        html += '</li>\n'
    html += '</ul>\n'
    return html

def f_olist(tree):
    #l = tree.height()/2
    l = 1
    html = ''

    m = re.match('sect(\d)', tree.parent.type)
    s = int(m.group(1)) - 2 #section level
    l = s + l
    for i, c in enumerate(tree.children):
        html += '<div class="level%s">\n' % str(l)
        #import pdb
        #pdb.set_trace()
        html += '<p class="list%s">%s%s' % (l,
                                           f_number(l, i),
                                           c[0].value)
        html += '</p>\n'
        for np in c.children[1:-1]:
            html += disp(np)
        html += '</div>\n'
    return html

def f_dlist(tree):
    html = '<dl>\n'
    for c in tree.children:
        html += '<dt>%s</dt>\n' % c.value
        html += '<dd>'
        for np in c.children:
            html += disp(np)
        html += '</dd>'
    html += '</dl>\n'
    return html

def f_footnotes(tree):
    html = '<div class="footnotes">\n'
    for c in tree.children:
        html += c.value+ '<br/>\n'
    html += '</div>\n'
    return html

def f_comment(tree):
    html = '<div class="title">%s</div>\n' % tree.value
    for c in tree.children: html += disp(c)
    html = '<div class="%s">\n%s\n</div>\n' % (tree.type, html)
    return html


def f_questions(tree):
    html = '<h3>習題</h3>\n'
    for i in range(0,len(tree.children)):
        c1 = tree.children[i]
        html += '<h4>題%s：%s</h4>\n' % (i+1, c1.title)
        for c2 in c1.children:
            html += disp(c2)
    return html

def f_answer(tree):
    html = '<h4>答：</h4>\n'
    for c in tree.children:
        html += disp(c)
    return html

def usage():
    msg = u'USAGE:' + os.path.basename(sys.argv[0]) + " stxt" + '\n'
    return msg

if __name__ == '__main__':
    print to_html(sys.argv[1])
