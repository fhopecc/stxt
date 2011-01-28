import os docs = [#r'doc\db\db.stx', 
        r'doc\taichi\taichi.stx'#, 
        #r'doc\poems\poems.stx',
        #r'doc\net\net.stx',
        #r'doc\csec\csec.stx',
        #r'doc\stable_prog\stable_prog.stx', 
        #r'doc\food\food.stx', 
        #r'doc\novel\novel.stx', 
        #r'doc\ds\ds.stx', 
        #r'doc\python\python.stx'
        ]
for doc in docs:
    print 'Transform %s' % doc
    target_dir = os.path.dirname(doc.replace('doc', 'structedtext'))
    os.system(r'del %s\*.html' % target_dir)
    os.system(r'lib\stxt\stxt.py -f web %s' % doc)

os.system(r'echo UPDATE TO SERVER')
os.system(r'pause')
os.system(r'appcfg.py update structedtext')

#os.system(r'appcfg.py update structedtext')
#for slide in slides:
#    print 'transform %s' % slide
#    dir = os.path.basename(slide)
#    dir = os.path.splitext(dir)[0]
#    os.system(r'lib\slides_outputter.py %s %s' % (dir, slide))
#    os.system(r'lib\slides_print_outputter.py %s %s' % (dir, slide))
