%%%-----------------------------------------------------------------------------
%%% @author s@shuvatov.ru
%%% @copyright 2018 Sergei Shuvatov
%%% @doc
%%%  Native implemented queue with TTL.
%%%  By default queue type is FIFO and TTL is 0 (disabled), size unlimited.
%%%  Usage:
%%%   {ok, Q} = enq:new([fifo,
%%%                      {ttl, 10000}, % 10 seconds
%%%                      {max_size, 1000}]), % maximum 1000 elements
%%%   ok = enq:push(Q, test), % push atom 'test' to the queue
%%%   [test] = enq:pop(Q), % pop one element from the queue
%%%   [] = enq:pop(Q), % pop returns empty list if the queue is empty
%%%   % pushed item can be any term
%%%   ok = enq:push(Q, fun() -> io:format("some important job~n") end),
%%%   1 = enq:size(Q), % you can take length of the queue as efficiently as O(1)
%%% @end
%%%-----------------------------------------------------------------------------
-module(enq).
-author("Sergei Shuvatov").

%% API
-export([new/0,
         new/1,
         push/2,
         pop/1,
         size/1]).

-export_type([queue/0, option/0, error/0]).

-type queue() :: reference().
-type option() :: fifo |
                  lifo |
                  {ttl, Microseconds :: non_neg_integer()} |
                  {max_size, Count :: non_neg_integer()}.
-type error() :: max_size.

%%==============================================================================
%% API
%%==============================================================================

%% Same as enq:new([fifo, {ttl, 0}]).
-spec new() -> {ok, enq:queue()} | {error, enq:error()}.
new() ->
    new([]).

%% Returns a new queue or error in case of memory allocation error.
-spec new([option()]) -> {ok, enq:queue()} | {error, enq:error()}.
new(Options) ->
    enq_nif:new(Options).

%% Pushes Item on top (LIFO) or tail (FIFO) of Queue.
-spec push(Queue :: enq:queue(), Item :: any()) -> ok | {error, enq:error()}.
push(Queue, Item) ->
    enq_nif:push(Queue, erlang:term_to_binary(Item)).

%% Returns next item from the Queue.
-spec pop(Queue :: enq:queue()) -> [] | [any()].
pop(Queue) ->
    [ erlang:binary_to_term(I) || I <- enq_nif:pop(Queue) ].

%% Returns Queue length. Speed does not depend on number of elements.
-spec size(Queue :: enq:queue()) -> non_neg_integer().
size(Queue) ->
    enq_nif:size(Queue).

%%==============================================================================
%% Tests
%%==============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(log(F, A), io:format(standard_error, "~p:line ~p: " F "~n", [?FILE, ?LINE | A])).
-define(log(F), ?log(F, [])).

fifo_test() ->
    fifo_test(1000000).

fifo_test(N) ->
    {ok, Q} = enq:new(),
    T1 = erlang:timestamp(),
    % fill the queue with N elements
    fill(Q, N),
    Diff1 = timer:now_diff(erlang:timestamp(), T1),
    ?log("FIFO fill time: ~p ms", [Diff1 / 1000]),
    % ensure that size of queue matches N
    N = enq:size(Q),
    T2 = erlang:timestamp(),
    % pop all elements
    fifo_pop_all(Q, N),
    Diff2 = timer:now_diff(erlang:timestamp(), T2),
    ?log("FIFO pop time: ~p ms", [Diff2 / 1000]),
    % size of the queue must be 0
    0 = enq:size(Q).

fill(_Q, 0) ->
    ok;
fill(Q, N) ->
    ok = enq:push(Q, N),
    fill(Q, N - 1).

fifo_pop_all(Q, 0) ->
    [] = enq:pop(Q);
fifo_pop_all(Q, N) ->
    [N] = enq:pop(Q),
    fifo_pop_all(Q, N - 1).

ttl_test() ->
    {ok, Q} = enq:new([{ttl, 100}]),
    enq:push(Q, test),
    timer:sleep(95),
    [test] = enq:pop(Q),
    [] = enq:pop(Q),
    enq:push(Q, test),
    timer:sleep(105),
    [] = enq:pop(Q).

lifo_test() ->
    lifo_test(1000000).

lifo_test(N) ->
    {ok, Q} = enq:new([lifo]),
    T1 = erlang:timestamp(),
    % fill the queue with N elements
    fill(Q, N),
    Diff1 = timer:now_diff(erlang:timestamp(), T1),
    ?log("LIFO fill time: ~p ms", [Diff1 / 1000]),
    % ensure that size of queue matches N
    N = enq:size(Q),
    T2 = erlang:timestamp(),
    % pop all elements
    lifo_pop_all(Q, N),
    Diff2 = timer:now_diff(erlang:timestamp(), T2),
    ?log("LIFO pop time: ~p ms", [Diff2 / 1000]),
    % size of the queue must be 0
    0 = enq:size(Q).

lifo_pop_all(Q, N) ->
    lifo_pop_all(Q, 1, N).

lifo_pop_all(Q, I, N) when I > N ->
    [] = enq:pop(Q);
lifo_pop_all(Q, I, N) ->
    [I] = enq:pop(Q),
    lifo_pop_all(Q, I + 1, N).

max_size_test() ->
    {ok, Q} = enq:new([{ttl, 100}, {max_size, 1}]),
    ok = enq:push(Q, test),
    timer:sleep(50),
    {error, max_size} = enq:push(Q, 123),
    timer:sleep(55),
    ok = enq:push(Q, 321),
    [321] = enq:pop(Q),
    [] = enq:pop(Q).

-endif. % TEST