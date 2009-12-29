# coding=utf-8
from __future__ import with_statement
import sys, os
#sys.path.append(os.path.dirname(__file__).replace(r'lib','config'))
import pymssql
class NETDB(object):
  def __init__(self):
    self.conn = pymssql.connect(host='netdb', user='eltweb', \
      password='bewt_111', database='yrxweb')
  def cursor(self):return self.conn.cursor()

def OutputTable(FileName, TableName):
  netdb = NETDB() #宣告 netdb 物件
  c = netdb.cursor()

  #輸出為檔案
  fn=r'd:\work\%s_%s' % (TableName, FileName)
  with open(fn,'w') as f:

    #列印標題欄
    c.execute("select name from syscolumns where id=object_id('%s')" \
              % TableName)
    row = c.fetchone()
    while row:
      f.write("\t"+row[0]+",")
      row = c.fetchone()
    f.write("\n")

    #列印內容
    c.execute("select * from %s" % TableName)
    row = c.fetchone()
    while row:
      for rdata in row:
        f.write("\t"+str(rdata)+",")
      f.write("\n")
      row = c.fetchone()

OutputTable("out3.csv","yrxt010")
if __name__ == '__main__':
  try:
    FileName, TableName = sys.argv[1], sys.argv[2]
    if FileName[-4:]!=".csv":  # 避免忘記加副檔名
      FileName += ".csv"
    OutputTable(FileName,TableName)
    ms = r'已把表格 %s 儲存至 D:\work\%s_%s' % \
        (TableName,TableName,FileName)
    print ms.decode('utf8').encode('cp950')
    os.system(r'd:\work\%s_%s' % (TableName, FileName))
    #os.system(r"explorer /e, d:\work\ ")
  except IndexError:
    usage = '用法:' + os.path.basename(sys.argv[0]) + ' 輸出之檔案名稱 輸入之表格名稱\n'\
        '例如:' + os.path.basename(sys.argv[0]) + ' Output.csv YRXT346'
    print usage.decode('utf8').encode('cp950')
