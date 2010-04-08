from os import path
import sys, os

def set_sysenv(var, val):
    cmd = 'setx %s "%s" /M >nul 2>nul' % (var, val)
    if not os.system(cmd) == 0:
        raise 'set_sysenv("%s", "%s") failed!' % (var, val)

def add_path(p):
    if p not in os.environ['PATH'].split(';'):
        set_sysenv('PATH', r"%PATH%;" + p)
        print 'Add path "%s"' % p

'''
	def uniq_path
		upaths = ENV['PATH'].split(/;/).uniq 
    set_sysenv 'PATH', "#{upaths.join(';')}"
	  puts "PATH has unique its elements successfully!"
	end

	def del_path path
		paths = ENV['PATH'].split(/;/)
		if paths.include? path
			paths.delete path
      set_sysenv 'PATH', "#{paths.join(';')}"
			puts "Path [#{path}] has removed from PATH envvar successfully!"
		else
			puts "Path [#{path}] is not in PATH envvar!"
		end
	end
'''
add_path(path.dirname(sys.argv[0]))
