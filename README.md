# PubSub-over-Webhooks with RabbitHub

[RabbitHub][gitrepo] is an implementation of
[PubSubHubBub][pshb_project], a straightforward pubsub layer on top of
plain old HTTP POST - pubsub over Webhooks. RabbitHub provides an
HTTP-based interface to [RabbitMQ][].

It gives every AMQP exchange and queue hosted by a RabbitMQ broker a
couple of URLs: one to use for delivering messages to the exchange or
queue, and one to use to subscribe to messages forwarded on by the
exchange or queue. You subscribe with a callback URL, so when messages
arrive, RabbitHub POSTs them on to your callback. For example,

 - <http://dev.rabbitmq.com/rabbithub/endpoint/x/amq.direct> is the
   URL for delivering messages to the "amq.direct" exchange on the
   public test instance of RabbitMQ, and

 - <http://dev.rabbitmq.com/rabbithub/subscribe/q/some_queue_name> is
   the URL for subscribing to messages from the (hypothetical) queue
   "some_queue_name" on the broker.

The symmetrical .../subscribe/x/... and .../endpoint/q/... also exist.

The [PubSubHubBub protocol][spec] specifies some RESTful(ish)
operations for establishing subscriptions between message sources
(a.k.a "topics") and message sinks. RabbitHub implements these
operations as well as a few more for RESTfully creating and deleting
exchanges and queues.

While PubSubHubBub is written assuming Atom content, RabbitHub is
content-agnostic (just like RabbitMQ): any content at all can be sent
using RabbitHub's implementation of the PubSubHubBub protocol. Because
RabbitHub is content-agnostic, it doesn't implement any of the
Atom-specific parts of the PubSubHubBub protocol, including the "ping"
operation that tells a PSHB hub to re-fetch content feeds.

## <a name="example"></a>Example: combining HTTP messaging with AMQP and XMPP

Combining RabbitHub with the AMQP protocol implemented by RabbitMQ
itself and with the other adapters and gateways that form part of the
RabbitMQ universe lets you send messages across different kinds of
message networks - for example, our public RabbitMQ instance,
`dev.rabbitmq.com`, has RabbitHub running as well as the standard AMQP
adapter, the [rabbitmq-xmpp][] plugin, and a bunch of our other
experimental stuff, so you can do things like this:

<img src="http://github.com/tonyg/rabbithub/raw/master/doc/rabbithub-example.png" alt="RabbitHub example configuration"/>

 - become XMPP friends with `pshb@dev.rabbitmq.com` (the XMPP adapter
   gives each exchange a JID of its own)

 - use PubSubHubBub to subscribe the sink
   <http://dev.rabbitmq.com/rabbithub/endpoint/x/pshb> to some
   PubSubHubBub source - perhaps one on the public Google PSHB
   instance. (Note how the given URL ends in "x/pshb", meaning the
   "pshb" exchange - which lines up with the JID we just became XMPP
   friends with.)

 - wait for changes to be signalled by Google's PSHB hub to RabbitHub

 - when they are, you get an XMPP IM from `pshb@dev.rabbitmq.com` with
   the Atom XML that the hub sent out as the body

Again, RabbitHub is content-agnostic, so the fact that Atom appears is
an artifact of what Google's public PSHB instance is mailing out,
rather than anything intrinsic in pubsub-over-webhooks.

## Installation

To install from source (requires Erlang R15B01 or higher):

    git clone https://github.com/brc859844/rabbithub
    cd rabbithub
    make deps
    make
    make package
    cp dist/*.ez $RABBITMQ_HOME/plugins

Note that Windows users can build the plugin using the commands under Cygwin. When working with Cygwin ensure that the Erlang bin directory is in your PATH (so that rebar can find erl and erlc) and that the zip utility is installed with your Cygwin installation (required to create the plugin ez file).

Enable the plugin:

    rabbitmq-plugins enable rabbithub

By default the plugin will listen for HTTP requests on port 15670.

Note that if no username is specified for HTTP requests submitted to RabbitHub then RabbitHub checks to see whether a default username has been specified for the rabbithub application, and if so uses it. By default RabbitHub is configured to use a default username of `guest` (see the definition of `default_username` in `rabbithub.app`). This configuration might be reasonable for development and testing (aside from security testing); however for production environments this will most likely not be ideal, and the default username should therefore be deleted or changed to a RabbitMQ username that has only the required permissions. It is generally also a good idea to disable the RabbitMQ `guest` user, or to at least reduce the permissions of `guest` (when RabbitMQ is initially installed, the username `guest` has full permissions and a rather well-known password).   

## HTTP messaging in the Browser

In order to push AMQP messages out to a webpage running in a browser,
try using <http://www.reversehttp.net/> to run a PubSubHubBub endpoint
in a webpage - see for instance
<http://www.reversehttp.net/demos/endpoint.html> and its [associated
Javascript](http://www.reversehttp.net/demos/endpoint.js) for a simple
prototype of the idea. It's also possible to build simple PSHB hubs in
Javascript using the same tools.

  [gitrepo]: http://github.com/tonyg/rabbithub
  [pshb_project]: http://code.google.com/p/pubsubhubbub/
  [spec]: http://pubsubhubbub.googlecode.com/svn/trunk/pubsubhubbub-core-0.1.html
  [RabbitMQ]: http://www.rabbitmq.com/
  [rabbitmq-xmpp]: http://hg.rabbitmq.com/rabbitmq-xmpp/raw-file/default/doc/overview-summary.html

## Proxy server support

If RabbitHub is being used behind a firewall, it may be necessary to route HTTP(s) requests to callback URLs via a proxy server. A proxy server can be specified for RabbitHub by defining `http_client_options` in `rabbitmq.config` as illustrated below, where the same proxy server has been specified for both HTTP and HTTPS, and the proxy server will not be used for requests to `localhost`.

    [
        {rabbithub, [
            {http_client_options, [
                {proxy,{{"10.1.1.1",8080}, ["localhost"]}},
                {https_proxy,{{"10.1.1.1",8080},["localhost"]}}
            ]}
        ]}
    ].

Note that proxy server support is only available in RabbitHub for RabbitMQ 3.2.1 or higher.

## Software License

RabbitHub is [open-source](http://www.opensource.org/) code, licensed
under the very liberal [MIT
license](http://www.opensource.org/licenses/mit-license.php):

    Copyright (c) 2009 Tony Garnock-Jones <tonyg@lshift.net>
    Copyright (c) 2009 LShift Ltd. <query@lshift.net>

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.
