import os
import csv
import sqlite3 as sqlite3

db = sqlite3.connect('cba.db')

for f in [x for x in os.listdir() if x.endswith('.csv')]:

    with open(f, newline='', encoding = 'utf-8-sig') as csvfile:
        data = csv.reader(csvfile, delimiter=',')
        tn = f.split('-')[2].split('.')[0] 
        isfirst = True

        for row in data:
            if isfirst:
                # cn for column name
                db.execute("create table %s (%s);" % 
                          (tn, ' text, '.join(map(lambda cn: '`%s`'%cn, row))))
                isfirst=False
            else:
                # v for value
                try:
                    db.execute("insert into %s values(%s)" % 
                              (tn, ', '.join(map(lambda v: "'%s'"%v, row))))
                except sqlite3.OperationalError:
                    with open("cba_import_error.log", 'a', newline='', encoding = 'utf-8-sig') as errorlog:
                        errorlog.write("insert into %s values(%s);\n" % 
                              (tn, ', '.join(map(lambda v: "'%s'"%v, row))))

db.commit()
db.close()
