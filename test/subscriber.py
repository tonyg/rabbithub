#! /usr/bin/python

import BaseHTTPServer
import urlparse

def run(secret="",
        verification_code="",
        server_class=BaseHTTPServer.HTTPServer):
    server_address = ('', 8888)

    class SubHandler(BaseHTTPServer.BaseHTTPRequestHandler):
        
        def do_GET(self):
            # Assume this is a verification request
            qs = urlparse.urlparse(self.path).query
            args = urlparse.parse_qs(qs)
            print "Verify: %s" % args
            assert(args['hub.mode']==["subscribe"])
            if ((verification_code=='' and not args.has_key('verify_token')) or
                args['verify_token']==[verification_code]):
                challenge = args['hub.challenge'][0]
                self.send_response(200, "OK")
                self.end_headers()
                self.wfile.write(challenge)
            else:
                self.send_response(404, "Subscription not found")
                self.end_headers()
            
            
        def do_POST(self):
            # Assume this is a delivery
            length = self.headers.getheader('Content-Length')
            payload = self.rfile.read(int(length))
            if secret == '':
                print "Delivery: %s" % payload
            else:
                sig = self.headers.getheader('X-Hub-Signature')
                print "Delivery: (%s) %s" % (sig, payload)
            self.send_response(202, "Accepted")
            self.end_headers()

    httpd = server_class(server_address, SubHandler)
    httpd.serve_forever()

if __name__ == '__main__':
    run()
