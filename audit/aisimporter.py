import os
import sys
import re
import csv
import xlrd
import sqlite3 as sqlite3
import jieba

# chinese_word_count
# read an text file and make a chinese word count table 
#
# arguments
# t:        text file to count the chinese word
# c:        a word count table in csv format
def chinese_word_count(t, c):
    ret = open(t, "r", encoding="utf-8").read()
    seglist = jieba.cut(ret, cut_all=False)
    hash = {}
    for item in seglist: 
        if item in hash:
            hash[item] += 1
        else:
            hash[item] =  1
    fd = open(c,"w", encoding="utf-8")
    fd.write("word,count\n")
    for k in hash:
        fd.write("%s,%d\n"%(k,hash[k]))


def parse_table_name(filename):
    pat = "\w+ *- *\d+\.(\w+) *- *\d+\.csv"
    m = re.match(pat, filename)
    if m == None:
        pat = "\w+-\d+-(\w+).csv"
        m = re.match(pat, filename)
    if m == None:
        print("wrong file name: %s" % filename)
    return m.group(1)

def is_duplicated_table(db, table):
    c = db.cursor()
    sql = "select count(*) from %s union select count(*) from (select distinct * from %s)" % (table, table)
    c.execute(sql)
    l = c.fetchall()
    if len(l)==2: print("duplicated table %s: %s, %s"%(table, l[0][0], l[1][0]))
    return (len(l) == 2)

def table_names(db):
    c = db.cursor()
    sql = "SELECT name FROM sqlite_master WHERE type='table' ORDER BY Name"
    c.execute(sql)
    return map(lambda x:x[0], c.fetchall())

def fill_code_name(dbf):
    db = sqlite3.connect(dbf)
    for tn in table_names(db):
        sql = ""
        if tn == '法定預算檔':
            print("表格「%s」補代碼名稱欄位"%tn)
            sql = """create table temp as select a.*, b.名稱 預算科目名稱, c.名稱 用途別名稱
                     from 法定預算檔 a join (select * from 基本代碼檔 where 代碼分類='1') b 
                     on a.機關編碼 = b.機關編碼
                     and a.送審月 = b.送審月
                     and a.預算科目代碼 = b.代碼 join (select * from 基本代碼檔 where 代碼分類='4') c
                     on a.機關編碼 = c.機關編碼
                     and a.送審月 = c.送審月
                     and a.用途別代碼 = c.代碼"""
        if tn == '明細分類帳':
            print("表格「%s」補代碼名稱欄位"%tn)
            sql = """select b.名稱 總帳科目名稱, c.名稱 用途別名稱, a.*
                     from 明細分類帳 a left outer join (select * from 基本代碼檔 where 代碼分類 in ('1', '2')) b 
                     on a.機關編碼 = b.機關編碼
                     and a.送審月 = b.送審月
                     and a.總帳科目 = b.代碼
                     left outer join (select * from 基本代碼檔 where 代碼分類='4') c
                     on a.機關編碼 = c.機關編碼
                     and a.送審月 = c.送審月
                     and a.用途別代碼 = c.代碼"""
        if sql != "":
            try: 
                #1.建立去除重覆紀錄之暫存表
                db.execute(sql)
                #2.刪除舊表
                sql = "drop table %s" % tn
                db.execute(sql)
                #3.將暫存表命名為新表
                sql = "alter table temp rename to %s" % tn
                db.execute(sql)
            except sqlite3.OperationalError as err:
                with open("error.log", 'a', newline='', encoding = 'utf-8-sig') as errorlog:
                    errorlog.write("{}".format(err))
                    errorlog.write(sql)
    db.commit()
    db.close()


def deduplicat_tables(dbf):
    db = sqlite3.connect(dbf)
    for tn in table_names(db):
        if is_duplicated_table(db, tn):
            print("表格「%s」去除重複紀錄"%tn)
            try: 
                #1.建立去除重覆紀錄之暫存表
                sql = "create table temp as select distinct * from %s" % tn
                db.execute(sql)
                #2.刪除舊表
                sql = "drop table %s" % tn
                db.execute(sql)
                #3.將暫存表命名為新表
                sql = "alter table temp rename to %s" % tn
                db.execute(sql)
            except sqlite3.OperationalError as err:
                with open("error.log", 'a', newline='', encoding = 'utf-8-sig') as errorlog:
                    errorlog.write("{}".format(err))
                    errorlog.write(sql)
    db.commit()
    db.close()

def drop_table(t, db):
    db = sqlite3.connect(db)
    sql = "DROP TABLE IF EXISTS %s;" % t
    db.execute(sql)
    db.close()

def import_dir_xls(directory, table_name, dbf
                  ,start_row=0 
                  ,end_row_str=''
                  ,debug=False
                  ):
    directory = os.path.abspath(directory)
    is_append=False
    for root, subdirs, files in os.walk(directory):
        for f in [x for x in files if x.endswith('.xls')]:
            fp = os.path.join(root, f)
            import_xls1(fp, table_name, dbf, is_append,
                        start_row=start_row, 
                        end_row_str=end_row_str,
                        debug=debug
                       )
            is_append=True

# import_xls1
# import a excel file into sqlite database.
#
# arguments
#   db:           filepath of database imported into.
#   f:            filepath of xls imported.
#   t:            name of table imported into.
#   start_row:    the table the first row num 
# 
def import_xls1(f, t, db, 
                is_append=False, 
                has_caption=True, 
                sheet_index=0,
                start_row=0, 
                end_row_str='', 
                debug=False
               ):
    db = sqlite3.connect(db)
    try:
        book = xlrd.open_workbook(f)
    except xlrd.biffh.XLRDError as err:
        print(f)
        print(err)
        return
    sheet = book.sheet_by_index(sheet_index)
    if debug: print(f)
    for r in range(start_row, sheet.nrows): # r for row number
        if r == start_row and not is_append:
            # cn for column name
            try:
                if has_caption:
                    sql = "create table `%s` (%s);" % (t, ' text, '.join(map(lambda c: '`%s`'%sheet.cell(r, c).value, range(0, sheet.ncols))))
                else:
                    sql = "create table `%s` (%s);" % (t, ' text, '.join(map(lambda c: 'f%d'%c, range(0, sheet.ncols))))
                print(sql)
                db.execute(sql)
            except sqlite3.OperationalError as err:
                with open("error.log", 'a', newline='', encoding = 'utf-8-sig') as errorlog:
                    errorlog.write("{}".format(err))
                    errorlog.write(sql)
        elif has_caption and r <= start_row+1:
            continue
        else:
            # v for value
            try:
                if str(sheet.cell(r, 0).value) == end_row_str: break
                sql = "insert into %s values(%s)" % (t, ', '.join(map(lambda c:"'%s'"%sheet.cell(r, c).value, range(0, sheet.ncols))))
                db.execute(sql)
            except sqlite3.OperationalError as err:
                with open("error.log", 'a', newline='', encoding = 'utf-8-sig') as errorlog:
                    errorlog.write("{}".format(err))
                    errorlog.write(sql)
    db.commit()
    db.close()

def import_gba_xls(db):
    db = sqlite3.connect(db)
    for f in [x for x in os.listdir() if x.endswith('.xls')]:
        book = xlrd.open_workbook(f)
        sheet = book.sheet_by_index(0)
        tn = f.split('-')[1].split('.')[1] #table name

        for r in range(0, sheet.nrows): # r for row number
            if r == 0:
                # cn for column name
                try:
                    sql = "create table `%s` (%s);" % (tn, ' text, '.join(map(lambda c: '`%s`'%sheet.cell(r, c).value, range(0, sheet.ncols))))
                    print(sql)
                    db.execute(sql)
                except sqlite3.OperationalError as err:
                    with open("error.log", 'a', newline='', encoding = 'utf-8-sig') as errorlog:
                        errorlog.write("{}".format(err))
                        errorlog.write(sql)
            else:
                # v for value
                try:
                    sql = "insert into `%s` values(%s)" % (tn, ', '.join(map(lambda c:"'%s'"%sheet.cell(r, c).value, range(0, sheet.ncols))))
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
            tn = parse_table_name(f)
            isfirst = True

            for row in data:
                try:
                    sql = ""
                    if isfirst:
                        # cn for column name
                        sql = "create table %s (%s);" % (tn, ' text, '.join(map(lambda cn: '`%s`'%cn, row)))
                        db.execute(sql)
                        isfirst = False
                    else:
                        # v for value
                        sql = "insert into %s values(%s)" % (tn, ', '.join(map(lambda v: "'%s'"%v.strip(), row)))
                        db.execute(sql)
                except sqlite3.OperationalError as e:
                    with open("error.log", 'a', newline='', encoding = 'utf-8-sig') as errorlog:
                        errorlog.write("msg %s: %s" % (e.strerror, sql))
    db.commit()
    db.close()

def import_csv2(f, table_name, db, is_append=False):
    db = sqlite3.connect(db)

    if table_name == '':
        table_name = os.path.splitext("path_to_file")[0]

    with open(f, newline='', encoding = 'utf-8-sig') as csvfile:
        data = csv.reader(csvfile, delimiter=',')
        isfirst = True
        for row in data:
            try:
                sql = ""
                if isfirst and not is_append:
                    # cn for column name
                    sql = "create table %s (%s);" % (table_name, ' text, '.join(map(lambda cn: '`%s`'%cn, row)))
                    db.execute(sql)
                    isfirst = False
                else:
                    # v for value
                    sql = "insert into %s values(%s)" % (table_name, ', '.join(map(lambda v: "'%s'"%v.strip(), row)))
                    db.execute(sql)
            except sqlite3.OperationalError as e:
                with open("error.log", 'a', newline='', encoding = 'utf-8-sig') as errorlog:
                    errorlog.write("msg %s: %s" % (e.strerror, sql))
    db.commit()
    db.close()

#import_csv2('opendata99.csv', 'death', 'death.db', False)
#import_csv2('opendata97.csv', 'death', 'death.db', True)
#import_csv2('opendata98.csv', 'death', 'death.db', True)
#import_csv2('opendata100.csv', 'death', 'death.db', True)
#import_csv2('opendata101.csv', 'death', 'death.db', True)
#import_csv2('opendata102.csv', 'death', 'death.db', True)
#import_csv2('opendata103.csv', 'death', 'death.db', True)
#import_csv2('opendata104.csv', 'death', 'death.db', True)
#import_csv2('opendata105.csv', 'death', 'death.db', True)
#import_csv2('opendata106.csv', 'death', 'death.db', True)
#import_xls('ais.db')
#import_csv('ais.db')
#deduplicat_tables('ais.db')
#fill_code_name('ais.db')
