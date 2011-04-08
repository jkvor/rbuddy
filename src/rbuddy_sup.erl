
-module(rbuddy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SHost = env_var("RBUDDY_SERVER_HOST"),
    SPort = list_to_integer(env_var("RBUDDY_SERVER_PORT")),
    {ok, {{one_for_one, 5, 10}, [
        {rbuddy_tcp_proxy_sup, {rbuddy_tcp_proxy_sup, start_link, []}, permanent, 5000, worker, [rbuddy_tcp_proxy_sup]},
        {rbuddy_tcp_listener, {rbuddy_tcp_listener, start_link, [SHost, SPort]}, permanent, 5000, worker, [rbuddy_tcp_listener]}
    ]}}.

env_var(Key) ->
    case os:getenv(Key) of
        false ->
            error_logger:error_msg("Could not read envvar ~p~n", [Key]),
            exit(env_missing);
        Val ->
            Val
    end.
