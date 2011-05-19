-module(math_app_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

math_app_test_() ->
    [ math_monoid_tests:tests()
    ].
