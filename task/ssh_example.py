import sys, os, select
import paramiko

debug = True

username = 'test'
password ='gheghejjj'
hostname = '10.245.24.134'
hostport = 22

def run(t, cmd):
	'Open channel on transport, run command, capture output and return'
	global debug
	out = ''

	if debug: print 'DEBUG: Running cmd:', cmd
	chan = t.open_session()
    #chan.setblocking(0)

	if not chan.exec_command(cmd):
		raise 'ERROR: Failed to run command', cmd
    

	chan.close()
	return out

### Unbuffered sys.stdout
sys.stdout = os.fdopen(1, 'w', 0)

if debug:
	print 'DEBUG: Writing log to ssh-cmd.log'
	paramiko.util.log_to_file('ssh-cmd.log')

### Open SSH transport
t = paramiko.Transport((hostname, hostport))
t.connect(username=username, password=password)

print run(t, 'ls -la /')
print run(t, 'find /tmp')

t.close()
