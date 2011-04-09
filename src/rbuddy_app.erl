-module(rbuddy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, app_var/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rbuddy_sup:start_link().

stop(_State) ->
    ok.

app_var(Key) when is_atom(Key) ->
    case application:get_env(rbuddy, Key) of
        undefined ->
            error_logger:error_msg("Could not read app var ~p~n", [Key]),
            exit(missing_app_var);
        {ok, Val} ->
            Val
    end.
