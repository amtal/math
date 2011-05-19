-module(math_app_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

math_app_test_() ->
    [ fun math_monoid_tests:test/0
    ].
