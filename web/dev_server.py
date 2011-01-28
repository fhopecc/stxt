from wsgiref.simple_server import make_server
from lib import rest

from paste.exceptions.errormiddleware import ErrorMiddleware

app = ErrorMiddleware(rest.route, debug=True)

httpd = make_server('', 8080, app)

print "Serving HTTP on port 8080..."

# Respond to requests until process is killed
httpd.serve_forever()

# Alternative: serve one request, then exit
##httpd.handle_request()
