from calendar import Calendar
c = Calendar()
temp = '''
<table id="canlendar">
</table>
'''
for w in c.monthdatescalendar(2010, 5):
    print w
