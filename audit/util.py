import re

with open("cba.sql", encoding = 'utf-8-sig') as sql:
    sqlmap = {} 
    key = ""
    for line in sql:
        m = re.match('-- \d+\..*', line)
        if m: 
            key = m.group(0)
        else:
            try:
                sqlmap[key] = sqlmap[key] + line
            except KeyError:
                sqlmap[key] = line
    print(sqlmap)
