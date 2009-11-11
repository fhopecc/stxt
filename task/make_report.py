import os
docs = [r'doc\hltb\inner_audit\98\report.stx', 
        r'doc\hltb\inner_audit\99\plan.stx'
       ]
for doc in docs:
    bf = os.path.basename(doc) 
    print 'transform %s' % doc
    os.system(r'lib\chinese_number_outputter.py %s > %s.html' % (doc, bf))
