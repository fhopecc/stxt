import os
slides = [r'doc\db\db_slides.stx']
for slide in slides:
    print 'transform %s' % slide
    dir = os.path.basename(slide)
    dir = os.path.splitext(dir)[0]
    os.system(r'lib\slides_print_outputter.py %s %s' % (dir, slide))
