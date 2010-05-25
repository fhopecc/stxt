from calendar import Calendar
c = Calendar()
tdtemp = '''<td><div class="day">%s</div>
</td>
'''

trtemp = '''<tr>
%s
</tr>
'''


out = '''<table id="canlendar">'''
for w in c.monthdatescalendar(2010, 5):
    tr = '<tr>'
    for d in w:
        tr += tdtemp % d.day
    tr += '</tr>'
    out += tr
out += '</table>'
print out
