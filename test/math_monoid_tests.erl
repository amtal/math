-module(math_monoid_tests).
-export([test/0]).
-behaviour(math_monoid).
-export([mempty/0, mappend/2]).

%% A simple monoid. Mergeable min-max statistics.
%%
%% Note that the definition of mappend/2 ensures both values are of the same
%% type. (This can be useful if a heterogenous list is passed to concat by
%% accident.)
mempty() -> {?MODULE,undefined,undefined}.
mappend({?MODULE=M,A,B},{M,X,Y}) ->
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
    {ok,X3} = ?M:concat([X2|Vs]),
    % confirm min/max
    {?MODULE,1,5} = X3,
    % test empty concat - in a static typed language
    % it would return mempty, here it errors
    ok = try ?M:concat([]) catch error:badarg->ok end.
