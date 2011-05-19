-module(math_monoid_tests).
-export([test/0]).
-behaviour(math_monoid).
-export([mempty/0, mappend/2]).

%% A simple monoid. Mergeable min-max statistics.
mempty() -> {?MODULE,undefined,undefined}.
mappend({?MODULE,A,B},{?MODULE,X,Y}) ->
    {?MODULE,choose(A,X,min),choose(B,Y,max)}.
% where
    choose(undefined,X,_) -> X;
    choose(X,undefined,_) -> X;
    choose(X,Y,F) -> erlang:F(X,Y).

value(X) -> {?MODULE,X,X}.

-define(M,math_monoid).
test() ->
    % take some readings
    X1 = ?M:empty(?MODULE),
    X2 = ?M:append(value(5),X1),
    Vs = lists:map(fun value/1, [1,2,3,4,5]),
    X3 = ?M:concat([X2|Vs]),
    % confirm min/max
    {?MODULE,1,5} = X3,
    % test empty concat - in a static typed language
    % it would return mempty, here it soft-errors
    empty = ?M:concat([]).
