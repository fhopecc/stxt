import os
import csv
import sqlite3

db = sqlite3.connect('ais.db')

for fs in os.listdir():
    print(fs)

def get_table_name(fname):
    return fname.split('-')[2].split('.')[0]

f = '資料匯出-20180524090155-明細分類帳.csv'

with open(f, newline='', encoding = 'utf-8-sig') as csvfile:
    data = csv.reader(csvfile, delimiter=',')
    tn = get_table_name(f)
    isfirst = True

    for row in data:
        if isfirst:
            # cn for column name
            db.execute("create table %s (%s);" % 
                      (tn, ' text, '.join(map(lambda cn: '`%s`'%cn, row))))
            isfirst=False
        else:
            # v for value
            db.execute("insert into %s values(%s)" % 
                      (tn, ', '.join(map(lambda v: "'%s'"%v, row))))

db.commit()
db.close()
