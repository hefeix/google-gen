#!/usr/bin/python

import gen
import string,cgi,time
from os import curdir, sep
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

class MyHandler(BaseHTTPRequestHandler):
   def do_GET(self):
      params = {}
      parsed_path = self.path[1:]
      if (parsed_path[0:1] == "?"):
         parsed_path = parsed_path[1:]
      params = cgi.parse_qs(parsed_path)
      params_flat = {}
      for p in params:
         params_flat[p] = (params[p])[0]
      results = gen.ModelShellHandleExternal(params_flat)
      self.send_response(200)
      self.send_header('Content-type', 'text/html')
      self.end_headers()
      self.wfile.write(results['html'])
      self.wfile.write("<HR><PRE>\n")
      self.wfile.write(params_flat)
      self.wfile.write("<HR>")
      self.wfile.write(results['cerr'])
      self.wfile.write("</PRE>\n");
      return
           
def main():
    try:
        server = HTTPServer(('', 8080), MyHandler)
        print 'started httpserver...'
        server.serve_forever()
    except KeyboardInterrupt:
        print '^C received, shutting down server'
        server.socket.close()

if __name__ == '__main__':
    main()
