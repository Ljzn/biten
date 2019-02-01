#! /bin/sh

rebar3 compile && \

erl -pa _build/default/lib/*/ebin -boot start_sasl -s biten_app
