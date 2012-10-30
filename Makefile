all: compile

compile: deps/parse_trans
	@rebar compile

deps/parse_trans:
	@rebar get-deps

clean:
	@rebar clean

distclean:
	@rm -rf deps
	@rebar clean
