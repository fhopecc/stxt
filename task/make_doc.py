import os
docs = [r'doc\db\db.stx']
for doc in docs:
    print 'transform %s' % doc
    os.system(r'lib\web_outputter.py %s' % doc)
slides = [r'doc\db\db_slides.stx']
for slide in slides:
    print 'transform %s' % slide
    dir = os.path.basename(slide)
    dir = os.path.splitext(dir)[0]
    os.system(r'lib\slides_outputter.py %s %s' % (dir, slide))
