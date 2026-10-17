REBAR3 ?= rebar3

.PHONY: compile run tests dialyzer fmt fmt-check

compile:
	$(REBAR3) compile

run:
	$(REBAR3) shell

tests:
	$(REBAR3) ct

dialyzer:
	$(REBAR3) dialyzer

fmt:
	$(REBAR3) fmt

fmt-check:
	$(REBAR3) fmt --check
