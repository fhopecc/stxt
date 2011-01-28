import smtplib
msg = 'Hello world.'

server = smtplib.SMTP('smtp.gmail.com', 465) #port 465 or 587
mailserver.set_debuglevel(1)
server.ehlo()
server.starttls()
server.ehlo()
server.login('fhopecc@gmail.com','e271828')
server.sendmail('fhopecc@gmail.com','somename@somewhere.com',msg)
server.close()
