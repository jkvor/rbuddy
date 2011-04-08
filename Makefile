all: compile

compile:
	@./rebar compile

clean:
	@./rebar clean

get-deps:
	@./rebar get-deps

del-deps:
	@./rebar delete-deps

release: compile
	@./rebar generate

