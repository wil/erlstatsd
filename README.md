erlstatsd
=========

erlstatsd is a client implementation of Etsy's brilliant [statsd server][statsd] -- a front end/proxy for the [Graphite][graphite] stats collection and graphing server.


Usage
-----

Build it

    $ ./rebar compile

Run it

    $ erl -pa ebin
    1> application:start(erlstatsd).
    ok
    2> erlstatsd:increment("test.foo.bar", 1, 0.05).
    ok
    3> erlstatsd:timing("test.foo.proctime", 51, 0.5).
    ok

Specify options

    $ erl -pa ebin -erlstatsd statsd_host '"stats.myhost.net"'


Credits
-------
This README file was shamelessly ripped from Steve Ivy's [pystatsd][pystatsd] project.


[pystatsd]: https://github.com/sivy/py-statsd
[graphite]: http://graphite.wikidot.com
[statsd]: https://github.com/etsy/statsd
