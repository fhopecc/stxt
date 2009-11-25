from wsgiref.simple_server import make_server
from lib import rest

from paste.exceptions.errormiddleware import ErrorMiddleware
from paste.session import SessionMiddleware

app = ErrorMiddleware(rest.route, debug=True)
app = SessionMiddleware(app)

httpd = make_server('', 80, app)

print "Serving HTTP on port 80..."

# Respond to requests until process is killed
httpd.serve_forever()

# Alternative: serve one request, then exit
##httpd.handle_request()
