%% Copyright (c) 2011 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(rbuddy).
-behaviour(gen_server).

%% API
-export([start_link/0, active/0, standby/0, master/0,
         start_failover/0, complete_failover/0, update_status/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {listener, active, standby, master, status}).

-define(TIMEOUT, 10000).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

active() ->
    gen_server:call(?MODULE, active, ?TIMEOUT).

standby() ->
    gen_server:call(?MODULE, standby, ?TIMEOUT).

master() ->
    gen_server:call(?MODULE, master, ?TIMEOUT).

start_failover() ->
    gen_server:call(?MODULE, start_failover, ?TIMEOUT).

complete_failover() ->
    gen_server:call(?MODULE, complete_failover, ?TIMEOUT).

update_status(Status) when is_atom(Status) ->
    gen_server:cast(?MODULE, {status, Status}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    [Active, Standby] = get_slaves(), 
    Master = get_master(),
    Listener = get_listener(),
    {ok, #state{listener=Listener,
                active=Active,
                standby=Standby,
                master=Master,
                status=starting}, 0}.

handle_call(active, _From, #state{active=Active}=State) ->
    {reply, Active, State};

handle_call(standby, _From, #state{standby=Standby}=State) ->
    {reply, Standby, State};

handle_call(master, _From, #state{master=Master}=State) ->
    {reply, Master, State};

handle_call(start_failover, _From, #state{listener=Listener, active=Active, standby=Standby}=State) ->
    case start_failover(Listener, Active, Standby) of
        ok ->
            {reply, ok, State#state{status=failing_over}};
        Error ->
            {stop, Error, State}
    end;

handle_call(complete_failover, _From, #state{active=Active, standby=Standby}=State) ->
    do_notify(Standby),
    write_slaves_manifest(Standby, Active),
    {reply, ok, State#state{active=Standby, standby=Active, status=up}};
    
handle_call(_Msg, _From, State) ->
    {reply, invalid_msg, State}.

handle_cast({status, Status}, State) ->
    {noreply, State#state{status=Status}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{listener=Listener, active=Active, standby=Standby}=State) ->
    case start_failover(Listener, Active, Standby) of
        ok ->
            {noreply, State#state{status=failing_over}};
        Error ->
            {stop, Error, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(shutdown, _State) ->
    ok;

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% internal functions
%%====================================================================
get_slaves() ->
    case file:consult(get_slaves_manifest()) of
        {ok, [{H1,P1},{H2,P2}]} when is_list(H1), is_integer(P1), is_list(H2), is_integer(P2) ->
            [{H1, P1}, {H2, P2}];
        Other ->
            error_logger:error_msg("Could not read slaves.manifest ~p~n", [Other]), 
            exit(invalid_slaves_manifest)
    end.

get_slaves_manifest() ->
    filename:join("priv", "slaves.manifest").

get_master() ->
    case rbuddy_app:app_var(master) of
        {Host, Port} when is_list(Host), is_integer(Port) ->
            {Host, Port};
        Other ->
            error_logger:error_msg("Could not read master app var ~p~n", [Other]), 
            exit(invalid_app_var)
    end.

get_listener() ->
    case rbuddy_app:app_var(listener) of
        {Host, Port} when is_list(Host), is_integer(Port) ->
            {Host, Port};
        Other ->
            error_logger:error_msg("Could not read rbuddy app var ~p~n", [Other]), 
            exit(invalid_app_var)
    end.

get_notify_api() ->
    case rbuddy_app:app_var(notify_api) of
        {Host, Port} when is_list(Host), is_integer(Port) ->
            {Host, Port};
        Other ->
            error_logger:error_msg("Could not read notify_api app var ~p~n", [Other]), 
            exit(invalid_app_var)
    end.

start_failover(Listener, Active, Standby) ->
    error_logger:info_msg("Start failover~n"),
    case send_cmd(Active, rbuddy_redis_proto:slave_of("NO", "ONE")) of
        ok ->
            error_logger:info_msg("Set ~p -> NO ONE~n", [Active]),
            {LHost, LPort} = Listener,
            case send_cmd(Standby, rbuddy_redis_proto:slave_of(LHost, LPort)) of
                ok ->
                    error_logger:info_msg("Set ~p -> ~p ~p~n", [Standby, LHost, LPort]),
                    case rbuddy_slave_monitor_sup:start_child(Standby) of
                        {ok, _Pid} ->
                            ok;
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

send_cmd({Host, Port}, Cmd) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}]) of
        {ok, Socket} ->
            case rbuddy_redis:ok_cmd(Socket, Cmd) of
                ok ->
                    gen_tcp:close(Socket),
                    ok;
                Error ->
                    Error 
            end;
        Error ->
            Error
    end.

do_notify({SlHost, SlPort}) ->
    {ApiHost, ApiPort} = get_notify_api(),
    Url = lists:flatten(io_lib:format("http://~s:~w/failover", [ApiHost, ApiPort])),
    Post = lists:flatten(io_lib:format("host=~s&port=~w", [SlHost, SlPort])),
    error_logger:info_msg("POST ~s -> ~s~n", [Url, Post]),
    CT = "application/x-www-form-urlencoded",
    case httpc:request(post, {Url, [{"Content-Type", CT}], CT, Post}, [{timeout, 10000}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, Status, _}, _, Body}} ->
            error_logger:error_msg("HTTP failover request failed: status=~p body=~p", [Status, Body]);
        Error ->
            error_logger:error_report([?MODULE, ?LINE, Error])
    end.

write_slaves_manifest(Standby, Active) ->
    Manifest = get_slaves_manifest(),
    error_logger:info_msg("Write ~p~n", [Manifest]),
    {ok, FD} = file:open(Manifest, [write]),
    io:format(FD, "~p.~n", [Standby]),
    io:format(FD, "~p.~n", [Active]),
    file:close(FD).
    
