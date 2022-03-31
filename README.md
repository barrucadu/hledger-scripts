hledger-scripts
===============

## This repository is archived!

These scripts are pretty complicated.  This isn't ideal, but it's
necessary: InfluxDB 1 doesn't have a very good query language, so
`hledger-to-influxdb` needs to do a lot of work to mangle the data
into a format I can do what I want with.

But every time I want to do something new with the data, I need to go
change the script.  And occasionally the script fails because it times
out uploading all the data to InfluxDB.

I realised I could do much less pre-processing, dramatically
simplifying the script and making it more reliable, if I migrated to a
timeseries database with a more powerful query language.  The obvious
choices were InfluxDB 2 and Prometheus.

InfluxDB 2 is very different, and I don't use it at work or in any
other projects, I would be learning it just to create one dashboard.
That wasn't very appealing.

Prometheus is something I already use for monitoring my personal
things, and I use it at work too.  But I couldn't figure out how to
get my historic data into it.

Now I've solved that problem.  I found [promscale][], a
Prometheus-compatible timeseries database which supports bulk
importing old data.

So these scripts are **no longer used**.  Go see [my new, much
simpler, script][] which is written in Python and uses `hledger print
-O csv` rather than interfacing with the hledger library.

[promscale]: https://github.com/timescale/promscale
[my new, much simpler, script]: https://github.com/barrucadu/nixfiles/blob/ba59fce93c1bf615fde1a7556d39b5faebe29914/hosts/nyarlathotep/jobs/hledger-export-to-promscale.py
