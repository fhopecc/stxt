# coding=utf-8
from __future__ import with_statement
import sys, os, re, sqlite3
sys.path.append(os.path.join(os.path.dirname(__file__),'..'))
print sys.path
from lib import stxt_parser
stx = os.path.join(os.path.dirname(__file__),'..', 'doc', 'db', 'db.stx')
def import_sql_code():
    conn = sqlite3.connect('test.db')
    cur = conn.cursor()
 
    d = stxt_parser.parser.read(stx)
    cs = (c for c in d.dfs() 
            if      c.name is not None 
                and c.type == 'code' 
                and c.name.endswith('.sql'))

    for c in cs:
        cur.execute('''insert into 
                       sql_examples(name, sql) 
                       values(?,?)''', 
                       (c.title, c.value))

    conn.commit()

def create_table():
    conn = sqlite3.connect('test.db')
    cursor = conn.cursor()
    try:
        cursor.execute('drop table sql_examples');
    except sqlite3.OperationalError:
        pass

    cursor.execute('''create table 
                      sql_examples(id   integer, 
                                   name text, 
                                   sql  text, 
                                   primary key(id))''')

def usage():
    print "Extract the table into csv format." 
    print "USAGE:" + os.path.basename(__file__) + " stxt table_name"
    print "stxt: structed text source"
    print "table_name: table name to export"

if __name__ == '__main__':
    create_table()
    import_sql_code()
