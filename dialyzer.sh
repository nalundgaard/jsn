#!/bin/bash

# if we don't do this, deps will be masked by existing $ERL_LIBS setting which
# take precedence over -pa
export ERL_LIBS="`pwd`/deps:`pwd`/apps:$ERL_LIBS"

# The PLT of core Erlang libraries is presumed to exist
if [ -n "$DIALYZER_PLT" ]; then
    ARGS_PLT="--plts $DIALYZER_PLT"
else
    ARGS_PLT="--plts $HOME/.dialyzer_plt"
fi

# We also need to build a PLT of dependent projects so we can check for
# type-correctness of library calls. Dialyzer maintains this file when it
# becomes out of date.
DEP_PLT="./.dialyzer_deps_plt"
if [ -e "$DEP_PLT" ]; then
    ARGS_PLT="$ARGS_PLT $DEP_PLT"
else
    echo "Building PLT for dependent projects..."
    DEP_PROJS=`find . -regex '^\./deps/[^/]*/ebin$'; find . -regex '^\./apps/[^/]*/ebin$'`
    echo -e "dependent projects:\n$DEP_PROJS"
    if [ -n "$DEP_PROJS" ]; then
        dialyzer --quiet --build_plt --output_plt "$DEP_PLT" $DEP_PROJS
        if [ $? -ge 1 ]; then exit $?; fi
        ARGS_PLT="$ARGS_PLT $DEP_PLT"
    fi
fi

ARGS_WARN="-Werror_handling -Wunderspecs"

echo "Proceeding with ebin analysis..."

# Exclude warnings within generated modules
dialyzer \
    --no_native \
    $ARGS_WARN \
    $ARGS_PLT -- \
        ebin \
| grep -v '_piqi\.erl:[0-9]*: ' \
| tee ./dialyzer.output
