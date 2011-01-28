# coding=utf-8
from __future__ import with_statement
import sys, os, re
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
import booker
import template 
from template import Template 
import logging
import traceback

logging.basicConfig(level=logging.DEBUG,
                    filename='stxt.log',
                    filemode='w')
logger = logging.getLogger('stxt.formatter.html')

def disp(tree):
    try:
        if re.match(r'sect\d', tree.type):
            return f_titled_container(tree)
        elif tree.type in ('question', 'answer', 'define', 'theorem'):
            return f_titled_container(tree)
        elif tree.type in ('proof', 'term', 'quote'):
            return f_titled_container(tree)
        elif tree.type in ('emphasis', 'comment'):
            return f_inline_element(tree)
        elif tree.type in ('para', 'listitem', 'olistitem'):
            return f_container(tree)
        else: return globals()['f_' + tree.type](tree)
    except TypeError:
        msg = "Error at %s:%s:%s" % (tree.type, tree.file, tree.lineno)
        logger.error(msg)
        traceback.print_exc()
        exit()

def to_html(file):
    d = None
    d = booker.read(file)
    
    d.number_children()
    d.count_occurence()
    title = os.path.basename(file)

    render = template.render('template', globals={'css':f_css()})
    return str(render.single_html(title, disp(d)))

def f_css():
    f = open(os.path.join('structedtext', 'css', 'web.css'))
    css = f.read()
    f.close()
    return css

def f_doc(tree):
    html = ''
    for e in tree.children:
        html += disp(e)
    return html
    
def f_title(tree):
    title = ""
    if re.match(r'sect\d', tree.type):
        title = f_label(tree) #+ tree.title
    elif tree.type in ('question', 'define', 'theorem'):
        title =  f_label(tree) + "：" + tree.title
    elif tree.type in ('proof', 'answer'):
        title =  f_label(tree) + '：'
    elif tree.type in ('table', 'image', 'code'):
        title =  f_label(tree) + "：" + tree.title
    elif tree.type in ('term'):
        title = tree.title
    return title

def f_label(tree):
    type = {'define'  : '定義', 
            'theorem' : '定理', 
            'proof'   : '證明', 
            'code'    : '程式', 
            'question': '題', 
            'answer'  : '答', 
            'table'   : '表', 
            'image'   : '圖' 
            }
    label = ""
    if re.match(r'sect\d', tree.type):
        label = tree.title.replace(r'\n', '<br/>')
    elif tree.type in ('question', 'define', 'theorem'):
        label =  type[tree.type] + str(tree.order() + 1)
    elif tree.type in ('proof', 'answer'):
        label =  type[tree.type]
    elif tree.type in ('table', 'image', 'code'):
        label =  type[tree.type] + str(tree.order() + 1)
    elif tree.type in ('reference'):
        try:
            label = tree.reflabel
        except AttributeError:
            ref = tree.get(tree.refname) 
            label = f_label(ref)
    return label

def f_titled_container(tree):
    temp = '''$def with (id, type, title, content)
<div id="$id" class="$type"><div class="title">$title</div>
<div class="$:(type)_content">
$:content
</div>
</div>
'''
    content =""
    for c in tree.children: content += disp(c)
    temp = Template(temp)
    return str(temp(f_address(tree), tree.type, f_title(tree), content))

def f_image(tree):
    temp = '''$def with (type, title, path)
<div class="$type"><div class="title">$title</div>
<img src="images/$path" alt="$title"/>
</div>
'''
    temp = Template(temp)
    return str(temp(tree.type, f_title(tree), tree.name))

def f_flash(tree):
    temp = '''$def with (type, title, path)
<div class="$type"><div class="title">$title</div>
<object id="player" classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" name="player" width="400" height="315"> 
		<param name="movie" value="videos/player-viral.swf" /> 
		<param name="allowfullscreen" value="true" /> 
		<param name="allowscriptaccess" value="always" /> 
		<param name="flashvars"
        value="file=videos/video.flv&image=videos/preview.jpg" /> 
		<embed 
			type="application/x-shockwave-flash"
			id="player2"
			name="player2"
			src="videos/player-viral.swf" 
			width="400" 
			height="315"
			allowscriptaccess="always" 
			allowfullscreen="true"
			flashvars="file=videos/video.flv&image=videos/preview.jpg" 
		/> 
</object> 
</div>
'''
    temp = Template(temp)
    vurl = os.path.join('videos', tree.name.replace(' ', '_'))
    return str(temp(tree.type, f_title(tree), vurl))

def f_video(tree):
    temp = '''$def with (type, title, path)
<div class="$type"><div class="title">$title</div>
<OBJECT id="VIDEO" width="320" height="240" 
	CLASSID="CLSID:6BF52A52-394A-11d3-B153-00C04F79FAA6"
	type="application/x-oleobject">
	
	<PARAM NAME="URL" VALUE="$:path">
	<PARAM NAME="SendPlayStateChangeEvents" VALUE="True">
	<PARAM NAME="AutoStart" VALUE="FALSE">
	<PARAM name="uiMode" value="mini">
	<PARAM name="PlayCount" value="1">
	<PARAM name="fullScreen" value="True">
</OBJECT>

</div>
'''

#style="position:absolute; left:0;top:0;"

    temp = Template(temp)
    vurl = os.path.join('videos', tree.name.replace(' ', '_'))
    vurl = vurl + '.wmv'
    ext = os.path.splitext(vurl)[1]
    #if ext in ('.flv'):
    #    return f_flash(tree)
    #else:
    #    return str(temp(tree.type, f_title(tree), vurl))
    return str(temp(tree.type, f_title(tree), vurl))

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
                    if len(col.children) > 0:
                        content += '<td>%s</td>\n' % \
                                   disp(col[0])
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

def f_literal(tree):
    temp = '''$def with (type, literal)
<div class="$type">
<pre>
$:literal
</pre>
</div>
'''
    temp = Template(temp)
    return str(temp(tree.type, tree.value))

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

def f_cblock(tree):
    return tree.value

def f_inline_element(tree):
    temp = '''$def with (type, content)
<span class="$type">
$content
</span>
'''
    temp = Template(temp)
    return str(temp(tree.type, tree.value))

def f_element(tree):
    temp = '''$def with (type, content)
<div class="$type">
$content
</div>
'''
    temp = Template(temp)
    return str(temp(tree.type, tree.value))

def f_list(tree):
    temp = '''$def with (items)
<div class="list">
$for i in items:
    <div class="listitem">
    $for p in i.children:
        $if loop.first:
            <div class="para">
            * $(p.value)
            </div>
        $else:    
            $:(disp(p))
    </div>
</div>
'''
    globals = {'disp':disp}
    temp = Template(temp, globals=globals)
    return str(temp(tree.children))

def f_olist(tree):
    temp = '''$def with (items)
<div class="olist">
$for i in items:
    <div class="olistitem">
    $ index = loop.index
    $for p in i.children:
        $if loop.first:
            <div class="para">
            $index.
            $for c in p.children:
                $:(disp(c))
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
    level = tree.height()
    n = '%s.' % (tree.order() + 1)
    for l in range(1, level):
        tree = tree.parent
        if not tree: break
        n = '%s.%s' % (tree.order() + 1, n)
    return n

def f_insert(tree):
    insert = tree.get(tree.node_name)
    return disp(insert)

def f_reference(tree):
    temp = '''$def with (label, url)
<a class="reference" href="$url">$label</a>
'''
    try:
        ref = tree.reftree()
        temp = Template(temp)
        return str(temp(f_label(ref), f_url(ref)))
    except KeyError:
        logger.error("[%s] not in symbol table" % tree.address)
        return tree.address

def f_address(tree):
    type = tree.type  
    name = f_label(tree).replace('.', '_')
    if tree.name: 
        name = tree.name
    id = '%s_%s' % (type, name)
    return id.lower()

def f_url(tree):
    '轉換文件元素位址為對應之 URL'
    return '#%s' % f_address(tree)

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
    fn = sys.argv[1]
    print to_html(fn)
#console.info(usage()) 
