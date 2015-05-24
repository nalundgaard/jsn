#!/bin/sh
# ex: et sw=4 ts=4

ERL_LIBS=`pwd`/deps erl -config erl -pa ebin -boot start_sasl $*
