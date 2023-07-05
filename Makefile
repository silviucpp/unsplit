.PHONY: all compile clean deps eunit test docs doc

all: compile

compile: deps
	rebar3 compile

clean:
	rebar3 clean

deps:
	rebar3 get-deps
	rebar3 update-deps

eunit:
	rebar3 eunit

test:
	rebar3 ct

docs: doc
doc:
	rebar3 edoc


