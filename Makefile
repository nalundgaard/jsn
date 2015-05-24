REBAR=./rebar

.PHONY: all compile deps clean clean-deps test 


all: compile


deps:
	$(REBAR) get-deps


compile:
	$(REBAR) compile


test: eunit


eunit:
	ERL_AFLAGS="-config erl" $(REBAR) eunit skip_deps=true


dialyzer:
	./dialyzer.sh


clean:
	$(REBAR) clean
	rm -rf ebin dialyzer.output .eunit


clean-deps:
	$(REBAR) delete-deps
	rm -rf deps .dialyzer_deps_plt


fresh: clean clean-deps deps compile test
