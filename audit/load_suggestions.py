import sqlite3 as sqlite3
import urllib.request
import json
import os
import re

def escape_sql_string(str):
    return str.replace("'", "''")

def insert_page(rs, db):
    try:
        for r in rs:
            sql = """insert into suggestions( county ,suggestor_name ,bid_by 
,election_year ,suggest_year ,suggest_month ,suggestion   ,suggest_expense 
,approved_expense ,expend_on ,brought_by ,bid_type) values('%s'
,'%s' ,'%s' ,'%s' ,'%s' ,'%s' ,'%s' ,'%s' ,'%s' ,'%s' ,'%s' ,'%s')
""" % ( r['county'] ,r['suggestor_name'] ,",".join(r['bid_by']) ,r['election_year'] 
,r['suggest_year'] ,r['suggest_month'] ,r['suggestion']   ,r['suggest_expense'] 
,r['approved_expense'] ,r['expend_on'] ,r['brought_by'] ,r['bid_type'])
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
,election_year text
,suggest_year text
,suggest_month text
,suggestion text  
,suggest_expense text
,approved_expense text
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
