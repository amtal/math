-module(math_app_tests).
-export([all_tests_/0]).

all_tests() ->
    [ math_monoid_tests:test/0
    ].
