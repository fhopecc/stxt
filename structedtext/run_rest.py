import logging, os, sys
from google.appengine.ext.webapp import util
import rest

def main():
  # Run the WSGI CGI handler with that application.
  util.run_wsgi_app(rest.route)

if __name__ == '__main__':
  main()
