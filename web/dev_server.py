from wsgiref.simple_server import make_server, demo_app
from lib import rest

httpd = make_server('', 80, rest.route)
print "Serving HTTP on port 80..."

# Respond to requests until process is killed
httpd.serve_forever()

# Alternative: serve one request, then exit
##httpd.handle_request()
