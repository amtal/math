test: .eunit src/*
	rebar compile skip_deps=true;rebar eunit skip_deps=true

