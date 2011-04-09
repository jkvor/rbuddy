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
-module(rbuddy_slave_monitor).
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {slave, socket, notify}).

%%====================================================================
%% API functions
%%====================================================================
start_link(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Port]) ->
    {ApiHost, ApiPort} = get_notify_api(), 
    case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}]) of
        {ok, Socket} ->
            {ok, #state{slave={Host, Port}, socket=Socket, notify={ApiHost, ApiPort}}, 0};
        Error ->
            error_logger:error_report([?MODULE, ?LINE, Error]), 
            {stop, Error}
    end.

handle_call(_Msg, _From, State) ->
    {reply, invalid_msg, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{slave=Slave, socket=Socket, notify=Notify}=State) ->
    case rbuddy_redis:info(Socket) of
        Info when is_list(Info) ->
            LinkStatus = proplists:get_value("master_link_status", Info),
            Syncing = proplists:get_value("master_sync_in_progress", Info),
            case [LinkStatus, Syncing] of
                ["up", "0"] ->
                    do_notify(Notify, Slave),
                    {stop, normal, State};
                _ ->
                    {noreply, State, 250}
            end;
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
do_notify({ApiHost, ApiPort}, {SlHost, SlPort}) ->
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

get_notify_api() ->
    case rbuddy_app:app_var(notify_api) of
        {Host, Port} when is_list(Host), is_integer(Port) ->
            {Host, Port};
        Other ->
            error_logger:error_msg("Could not read notify_api app var ~p~n", [Other]), 
            exit(invalid_app_var)
    end.
