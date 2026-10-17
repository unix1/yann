REBAR3 ?= rebar3

.PHONY: compile run tests dialyzer

compile:
	$(REBAR3) compile

run:
	$(REBAR3) shell

tests:
	$(REBAR3) ct

dialyzer:
	$(REBAR3) dialyzer
