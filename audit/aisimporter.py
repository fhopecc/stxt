import os
import csv
import xlrd
import sqlite3 as sqlite3

def is_duplicated_table(db, table):
    c = db.cursor()
    sql = "select count(*) from %s union select count(*) from (select distinct * from %s)" % (table, table)
    c.execute(sql)
    l = len(c.fetchall())
    if l==2: print("duplicated table %s: %s, %s"%(table, l[0], l[1]))
    return (l == 2)

def table_names(db):
    c = db.cursor()
    sql = "SELECT name FROM sqlite_master WHERE type='table' ORDER BY Name"
    c.execute(sql)
    return map(lambda x:x[0], c.fetchall())

def deduplicat_tables(dbf):
    db = sqlite3.connect(dbf)
    for tn in table_names(db):
        if is_duplicated_table(db, tn):
            print("表格「%s」去除重複紀錄"%tn)
            try: 
                #1.建立去除重覆紀錄之暫存表
                sql = "create table temp as select distinct * from %s" % table
                db.execute(sql)
                #2.刪除舊表
                sql = "drop table %s" % table
                db.execute(sql)
                #3.將暫存表命名為新表
                sql = "alter table temp rename to %s" % table
                db.execute(sql)
            except sqlite3.OperationalError as err:
                with open("error.log", 'a', newline='', encoding = 'utf-8-sig') as errorlog:
                    errorlog.write("{}".format(err))
                    errorlog.write(sql)
    db.commit()
    db.close()

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

#import_xls('ais.db')
#import_csv('ais.db')
deduplicat_tables('ais.db')
