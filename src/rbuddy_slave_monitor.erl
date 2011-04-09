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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket}).

%%====================================================================
%% API functions
%%====================================================================
start_link(Slave) ->
    gen_server:start_link(?MODULE, [Slave], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([{Host, Port}]) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}]) of
        {ok, Socket} ->
            {ok, #state{socket=Socket}, 0};
        Error ->
            error_logger:error_report([?MODULE, ?LINE, Error]), 
            {stop, Error}
    end.

handle_call(_Msg, _From, State) ->
    {reply, invalid_msg, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% TODO: fail after a certail number of tries
handle_info(timeout, #state{socket=Socket}=State) ->
    case rbuddy_redis:info(Socket) of
        Info when is_list(Info) ->
            LinkStatus = proplists:get_value("master_link_status", Info),
            Syncing = proplists:get_value("master_sync_in_progress", Info),
            case [LinkStatus, Syncing] of
                ["up", "0"] ->
                    rbuddy:complete_failover(),
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
