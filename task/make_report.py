import os
docs = [r'doc\hltb\inner_audit\98\report.stx', 
        r'doc\hltb\inner_audit\98\review_minutes.stx', 
        r'doc\hltb\inner_audit\99\plan.stx', 
        r'doc\hltb\yrx_draws.stx', 
        r'doc\hltb\edb_report.stx', 
        r'doc\hltb\edb_plan.stx'
       ]
for doc in docs:
    bf = os.path.basename(doc) 
    print 'transform %s' % doc
    os.system(r'lib\chinese_number_outputter.py %s > %s.html' % (doc, bf))
