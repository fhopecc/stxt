# coding=utf-8
from __future__ import with_statement
import sys, os, re, unittest, booker
import html
from html import f_image
from html import f_css
from html import disp
from html import to_html
from html import Template
from html import template

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

def to_html(file):
    d = None
    d = booker.read(file)
    
    d.number_children()
    d.count_occurence()
    title = os.path.basename(file)

    render = template.render('template', globals={'css':f_css()})
    return str(render.chinese(title, disp(d)))

def f_number(level, number):
    #import pdb
    #pdb.set_trace()
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
    if level == 1:
        return digits[0][number] + '.'
    if level == 2:
        return digits[1][number] + '.'
    elif level == 3:
        return '（%s）' % digits[1][number]
    elif level == 4:
        return str(number) + '.'
    elif level > 4:
        return '(%s)' % str(number)

def f_olist(tree):
    temp = '''$def with (level, items)
<div class="olist$level">
$for i in items:
    <div class="olistitem">
    $ index = loop.index
    $for p in i.children:
        $if loop.first:
            <div class="para">
            $:(f_number(level, index))
            $for c in p.children:
                $:(disp(c))
            </div>
        $else:    
            $:(disp(p))
    </div>
</div>
''' 
    l = tree.height() / 2
    globals = {'disp':disp, 'f_number': f_number}
    temp = Template(temp, globals=globals)
    return str(temp(l, tree.children))
 
def f_image(tree):
    temp = '''$def with (type, title, path)
<div class="$type">
<img src="images/$path" alt="$title"/>
</div>
'''
    temp = Template(temp)
    return str(temp(tree.type, tree.title, tree.name))

html.f_olist = f_olist
html.f_image = f_image

