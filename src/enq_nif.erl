%%%-------------------------------------------------------------------
%%% @author s@shuvatov.ru
%%% @copyright 2018 Sergei Shuvatov
%%%-------------------------------------------------------------------
-module(enq_nif).
-author("Sergei Shuvatov").

%% API
-export([new/1,
         push/2,
         pop/1,
         size/1]).

-on_load(load_nif/0).

-define(app, enq).
-define(log(F, A), io:format(standard_error, "~p:~p: " F, [?MODULE, ?LINE | A])).
-define(not_loaded(), not_loaded(?LINE)).

%%==============================================================================
%% API
%%==============================================================================

new(_Options) ->
    ?not_loaded().

push(_Queue, _Item) ->
    ?not_loaded().

pop(_Queue) ->
    ?not_loaded().

size(_Queue) ->
    ?not_loaded().

%%==============================================================================
%% Internal functions
%%==============================================================================

load_nif() ->
    SoName = get_priv_path(?MODULE),
    % ?log("Loading library: ~p ~n", [SoName]),
    ok = erlang:load_nif(SoName, 0).

get_priv_path(File) ->
    case code:priv_dir(get_app()) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join([filename:dirname(Ebin), "priv", File]);
        Dir ->
            filename:join(Dir, File)
    end.

get_app() ->
    case application:get_application(?MODULE) of
        {ok, App} ->
            App;
        _ ->
            ?app
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
