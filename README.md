## About

**redis buddy** is a tool that helps facilitate redis slave failover.

## Compile

    $ make get-deps
    $ make

## Run

    $ cp include/rbuddy.config.example include/rbuddy.config
    $ erl -pa ebin deps/*/ebin -config include/rbuddy
    1> application:start(sasl).
    2> application:start(rbuddy).

