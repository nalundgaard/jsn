REBAR ?= ./rebar3
REBAR_CMD = $(REBAR) $(profile:%=as %)

all: compile xref eunit

compile:
	@$(REBAR_CMD) compile

xref:
	@$(REBAR_CMD) xref

clean:
	@$(REBAR_CMD) clean

eunit:
	@$(REBAR_CMD) do eunit,cover

edoc:
	@$(REBAR_CMD) edoc

start: compile
	-@$(REBAR_CMD) shell

dialyze: compile
	@$(REBAR_CMD) dialyzer
