# coding=utf-8
from __future__ import with_statement
import sys, os, re, unittest, stxt_parser

# def num2BCNum(arabic):
#     _ChineseNumeric = '零壹貳參肆伍陸柒捌玖'
#     result = ''
#     bInZero = True; 
#     arabic = str(arabic); # 將數字轉成阿拉伯數字字串
#     if arabic[1] == '-':
#         isMinus = True; 
#         arabic = arabic[1:len(arabic)]
#     else 
#       isMinus = False; 
#       posOfDecimalPoint = arabic.find('.') # 取得小數點的位置 
# 
#      # 先處理整數的部分 
#     if posOfDecimalPoint == -1:
#         integerPartReverse = arabic[::-1] 
#     else: 
#         integerPartReverse := (arabic[0:posOfDecimalPoint])[::-1] 
# 
#     # 從個位數起以每四位數為一小節 
#     for iSection in range(0,(len(integerPartReverse)/4)+1):
#         sSectionArabic = integerPartReverse[iSection*4+1:iSection*4+1+4] 
#         sSection = ''
#         # 以下的 i 控制: 個十百千位四個位數 
#         for i in range(1, len(sSectionArabic)):
#             iDigit = int(sSectionArabic[i])
#             if iDigit = 0:
#             # 1. 避免 ’零’ 的重覆出現
#             # 2. 個位數的 0 不必轉成 ’零’
#                 if (not bInZero) and (i <> 1): 
#                     sSection = '零' + sSection
#                     bInZero = True 
#                 else:
#                     if i == 2:
#                         sSection = '拾' + sSection
#                     elif i == 3:
#                         sSection = '佰' + sSection
#                     elif i == 4:
#                         sSection = '仟' + sSection
#             sSection = _ChineseNumeric[iDigit] + sSection 
#             bInZero = False
# 
# (* 加上該小節的位數 *) 
# if len(sSection) = 0 then 
# begin 
# if (len(result) > 0) and (Copy(result, 1, 2) <> '零') then
# result :='零' + result;
# end 
# else 
# begin 
# case iSection of 
# 0: result := sSection; 
# 1: result := sSection + '萬' + result;
# 2: result := sSection +'億' + result;
# 3: result := sSection +'兆' + result;
# end; 
# end; 
# end; 
# 
# (* 處理小數點右邊的部分 *) 
# if posOfDecimalPoint > 0 then 
# begin 
# AppendStr(result, '點');
# for i := posOfDecimalPoint + 1 to len(arabic) do 
# begin 
# iDigit := Ord(arabic[i]) - 48; 
# AppendStr(result, Copy(_ChineseNumeric, 2 * iDigit + 1, 2)); 
# end; 
# end; 
# 
# (* 其他例外狀況的處理 *) 
# if len(result) = 0 then result := '零';
# if Copy(result, 1, 2) = '點' then result := '零' + result;
# 
# (* 是否為負數 *) 
# if isMinus then result := '負' + result;
# end;

def disp(tree):
    return globals()['f_' + tree.type](tree)

def make_sect_list(doctree):
    o = '主題列表<br/>'
    for sect1 in doctree.children:
        o += r'<a href="%s.html">%s%s</a><br/>' %\
                 (f_section_number(sect1), f_section_number(sect1), sect1.title)
    return o

def to_html(file):
    d = stxt_parser.parser.read(file)
    d.number_children()
    d.count_occurence()
    title = os.path.basename(file)
    with open(r'd:\stxt\template\chinese.html') as tfn:
        t = tfn.read()
        return t % {'title': title, 'content': disp(d)}

def f_section_number(tree):
    ns = tree.section_number(0)[0]
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

def f_book(tree):
    html = ''
    for sect1 in tree.children:
        html += disp(sect1)
    return html

def f_sect1(tree):
    html = '<h1>%s</h1>\n' % tree.title
    html += '<div class="sect1">'
    for c in tree.children:
        html += disp(c)
    html += '</div>'
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
        for np in c.children:
            html += disp(np)
        html += '</li>\n'
    html += '</ul>\n'
    return html

def f_olist(tree):
    html = '<ol>\n'
    for c in tree.children:
        html += '<li>' + c.value
        for np in c.children:
            html += disp(np)
        html += '</li>\n'
    html += '</ol>\n'
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
    html = '<ul>\n'
    for c in tree.children:
        html += '<li>' + c.value+ '</li>\n'
    html += '</ul>\n'
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
    msg = 'USAGE:' + os.path.basename(sys.argv[0]) + " stxt" + '\n'
    print msg

if __name__ == '__main__':
    try:
        print to_html(sys.argv[1])
    except IndexError:
        print msg
