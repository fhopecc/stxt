import sqlite3
import os

out = """\
var winners = [
"""

out_print = """\
var winners = [
"""

f = open("html\winners.js", "w", encoding="utf-8")
f2 = open("html\winners_print.js", "w", encoding="utf-8")

conn = sqlite3.connect('lottery.db')
sql = 'select * from candidates order by random() limit 43;'

for row in conn.execute(sql):
    out_print += '"' + row[1] + "," + row[0] + "," + row[2]  + "\",\n"
    out += '"' + row[1] + "," + row[0] + "\",\n"

out += "]"
out_print += "]"

f.write(out)
f2.write(out_print)
