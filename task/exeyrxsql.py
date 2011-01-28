# coding=utf-8
from optparse import OptionParser
import sys, os, pymssql
import pymssql
conn = pymssql.connect(host='netdb', user='eltweb', 
                       password='bewt_111', 
                       database='yrxweb')

def print_table(table):
    print u"表格 %s 結構如下：" % table

    cur = conn.cursor() 
    cur.execute("sp_columns %s" % table)
    des = cur.description
    line = " ".join([des[3][0], des[5][0], des[6][0], des[7][0]])
    print line

    row = cur.fetchone()
    while row:
      line = u""
      line = " ".join([str(row[3]), str(row[5]), str(row[6]), str(row[7])])
      print line
      row = cur.fetchone()

def exeyrxsql(sql):
    print u"於網路申報資料庫執行："
    print sql
    cur = conn.cursor() 
    cur.execute(sql)

    print u"成功"
   
    if cur.description:
        line = u""
        for des in cur.description:
            line += des[0] + " " 
        print line
    try:
        row = cur.fetchone()
        while row:
          line = u""
          for rdata in row:
            line += str(rdata) + " "
          print line
          row = cur.fetchone()
    except pymssql.OperationalError:
        pass

if __name__ == '__main__':
    usage = u"usage: %prog SQL"
    parser = OptionParser(usage)
    parser.add_option("-f", "--file", dest="file",
                      help=u"指定 sql 程式檔")
    parser.add_option("-t", "--table", dest="table",
                      help=u"印出指定 table 的結構")
    (options, args) = parser.parse_args()
    try:
        if options.table:
            print_table(options.table)
        else:
            exeyrxsql(args[0])
    except IndexError:
        parser.error('must have sql')
