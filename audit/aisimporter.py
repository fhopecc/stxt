import os
import csv
import xlrd
import sqlite3 as sqlite3

def import_xls(db):
    db = sqlite3.connect(db)
    for f in [x for x in os.listdir() if x.endswith('.xls')]:
        book = xlrd.open_workbook(f)
        sheet = book.sheet_by_name('gv_Analysis1')
        tn = f.split('-')[1].split('.')[1] #table name

        for r in range(0, sheet.nrows): # r for row number
            if r == 0:
                # cn for column name
                try:
                    sql = "create table %s (%s);" % (tn, ' text, '.join(map(lambda c: '`%s`'%sheet.cell(r, c).value, range(0, sheet.ncols))))
                    db.execute(sql)
                except sqlite3.OperationalError as err:
                    with open("error.log", 'a', newline='', encoding = 'utf-8-sig') as errorlog:
                        errorlog.write("{}".format(err))
                        errorlog.write(sql)
            else:
                # v for value
                try:
                    sql = "insert into %s values(%s)" % (tn, ', '.join(map(lambda c:"'%s'"%sheet.cell(r, c).value, range(0, sheet.ncols))))
                    db.execute(sql)
                except sqlite3.OperationalError as err:
                    with open("error.log", 'a', newline='', encoding = 'utf-8-sig') as errorlog:
                        errorlog.write("{}".format(err))
                        errorlog.write(sql)

    db.commit()
    db.close()

def import_csv(db):
    db = sqlite3.connect(db)

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
                        with open("error.log", 'a', newline='', encoding = 'utf-8-sig') as errorlog:
                            errorlog.write("insert into %s values(%s);\n" % 
                                  (tn, ', '.join(map(lambda v: "'%s'"%v, row))))

    db.commit()
    db.close()

import_xls('ais.db')
