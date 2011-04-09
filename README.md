## About

**redis buddy** is a tool that helps facilitate redis slave failover.

### The Scenario

* You have a redis master server and a redis slave replicating from your master.
* Your application reads from the slave redis.

### The Problem

* Due to a master redis failure or a network interruption, the connection is broken between master and slave.
* The slave stops receiving updates from the master and effectively goes into read-only mode.
* Eventually, the connection between master and slave is reestablished and the slave begins the sync process over again.
* While the slave is loading the rdb dump into memory, **your application can no longer access either the old dataset or new dataset**.
* Your application experiences a period of downtime proportional to the time it takes the slave to load the new dataset.

### The Solution

* Keep a clean standby slave sitting around (that is empty and in master mode).
* Place a proxy between the active slave and the master redis.
* When the proxy loses its connection to the master it does the following:
  * Issue a **SLAVEOF NO ONE** command to the active slave, putting it into read-only mode.
  * Issue a **SLAVEOF $PROXY\_HOST $PROXY\_PORT** command to the standby slave.
  * Accept a connection from the standby slave and attempt to re-connect to the master.
  * Notify interested parties of the failover event once the standby slave has been promoted to active.
  * Issue a **FLUSHDB** command to the old active slave, making it the new standby slave.

## Compile

    $ make get-deps
    $ make

## Run

    $ cp include/rbuddy.config.example include/rbuddy.config
    $ erl -pa ebin deps/*/ebin -config include/rbuddy
    1> application:start(sasl).
    2> application:start(rbuddy).

