#!/bin/sh

erl -pa ebin deps/*/ebin -config priv/rbuddy.config -boot start_sasl -eval "application:start(syslog), application:start(rbuddy)"
