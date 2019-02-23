import sqlite3 as sqlite3
import urllib.request
import json
import os
import re

def escape_sql_string(str):
    try:
        return str.replace("'", "''")
    except:
        return str

def insert_page(rs, db):
    try:
        for r in rs:
            sql = """insert into suggestions( county ,suggestor_name ,bid_by 
,election_year ,suggest_year ,suggest_month ,suggestion   ,suggest_expense 
,approved_expense ,expend_on ,brought_by ,bid_type) values('%s'
,'%s' ,'%s' ,'%s' ,'%s' ,'%s' ,'%s' ,'%s' ,'%s' ,'%s' ,'%s' ,'%s')
""" % ( escape_sql_string(r['county']) ,escape_sql_string(r['suggestor_name']) ,
        ",".join(list(map(escape_sql_string,r['bid_by']))) ,r['election_year'] 
,r['suggest_year'] ,r['suggest_month'] ,escape_sql_string(r['suggestion'])   ,r['suggest_expense'] 
,r['approved_expense'] ,escape_sql_string(r['expend_on']) ,escape_sql_string(r['brought_by']) ,
escape_sql_string(r['bid_type']))
            db.execute(sql)
        db.commit()
    except sqlite3.OperationalError as e:
        with open("error.log", 'a') as errorlog:
                errorlog.write('[-] Sqlite operational error: {}\n'.format(e))
                errorlog.write(sql)


if __name__ == '__main__':

    db = sqlite3.connect("suggestions.db")
    sql = """create table suggestions(county text
,suggestor_name text
,bid_by text
,election_year integer
,suggest_year integer
,suggest_month integer
,suggestion text  
,suggest_expense integer
,approved_expense integer
,expend_on text
,brought_by text
,bid_type text)
"""
    db.execute(sql)
 
    url = "https://councils.g0v.tw/api/suggestions/?format=json"
    while True:
        response = urllib.request.urlopen(url)
        data = response.read()      # a `bytes` object
        text = data.decode('utf-8') 
        rs = json.loads(text)
        insert_page(rs['results'], db)    
        url = rs['next']
        print(url)
        if not url:
            break
    db.close()
