#! /usr/bin/python

import BaseHTTPServer
import urlparse
import hmac
import hashlib

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
                csig = hmac.new(secret, payload, hashlib.sha1).hexdigest()
                if (sig==csig):
                    print "Signature accepted"
                    print "Delivery %s" % payload
                else:
                    print "Signature invalid: supplied %s, calculated %s" % (sig, csig)
            self.send_response(202, "Accepted")
            self.end_headers()

    httpd = server_class(server_address, SubHandler)
    httpd.serve_forever()

if __name__ == '__main__':
    from optparse import OptionParser
    p = OptionParser()
    p.add_option("-s", "--secret", dest="secret",
                 default='',
                 help="Secret with which to check delivery signatures")
    p.add_option("-t", "--token", dest="token",
                 default='',
                 help="The token with which to verify subscriptions")
    (options, args) = p.parse_args()
    run(secret=options.secret,
        verification_code=options.token)
