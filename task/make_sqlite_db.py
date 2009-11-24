# coding=utf-8
from __future__ import with_statement
import sys, os, re, sqlite3
sys.path.append(os.path.join(os.path.dirname(__file__),'..'))
print sys.path
from lib import stxt_parser
stx = os.path.join(os.path.dirname(__file__),'..', 'doc', 'db', 'db.stx')
def export_table(name):
    # make doctree
    conn = sqlite3.connect('test.db')
    c = conn.cursor()
    print stx
    d = stxt_parser.parser.read(stx)
    table = d.find_by_name(name)

    if table.children:
        for row in table.children:
            l = len(row.children)
            l -= 1
            sql = 'insert into %s values(?%s)' % \
            (name, ', ?' * l)
            print sql
            if row.children[0].type == 'th': continue
            vs = [col.value.encode('utf8') for col in row.children]
            #vs[0] = int(vs[0])
            c.execute(sql, vs)
    conn.commit()

def create_table(cursor):
    ts = ['departments', 'humans', 'tax_payments', 'employees', 
          'taxes', 'angles', 'masters']
    for t in ts:
        try:
            cursor.execute('drop table %s;' % t);
        except sqlite3.OperationalError:
            continue
    cursor.execute('CREATE TABLE departments(id integer, name text);')
    cursor.execute('CREATE TABLE employees(id integer, dep_id integer, \
                    boss_id integer,name text, gender text, title text);')
    cursor.execute('CREATE TABLE humans(id integer, name text, sex text, \
                     birthday text, primary key(id));')
    cursor.execute('CREATE TABLE tax_payments(id integer primary key, \
                    payer text, date text, area text, tax text,  \
                    amount integer);')
    cursor.execute('CREATE TABLE taxes(id integer primary key, name text);')
    cursor.execute('CREATE TABLE angles(name text);')
    cursor.execute('CREATE TABLE masters(name text);')

def usage():
    print "Extract the table into csv format." 
    print "USAGE:" + os.path.basename(__file__) + " stxt table_name"
    print "stxt: structed text source"
    print "table_name: table name to export"

if __name__ == '__main__':
    ts = ['departments', 'humans', 'tax_payments', 'employees', 
          'taxes', 'angles', 'masters']
    conn = sqlite3.connect('test.db')
    c = conn.cursor()
    create_table(c)
    for t in ts:
        export_table(t)
