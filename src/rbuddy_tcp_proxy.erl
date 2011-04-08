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
-module(rbuddy_tcp_proxy).
-behaviour(gen_server).

%% API
-export([start_link/0, attach/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {c_host, c_port, c_sock, s_host, s_port, s_sock}).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).

attach(Pid, Sock, Host, Port) ->
    gen_server:cast(Pid, {attach, Host, Port, Sock}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {reply, invalid_msg, State}.

handle_cast({attach, SHost, SPort, Sock}, State) ->
    {CHost, CPort} = rbuddy_tcp_utils:peerinfo(Sock),
    {noreply, State#state{s_host=SHost,
                          s_port=SPort,
                          c_host=CHost,
                          c_port=CPort,
                          c_sock=Sock}, 0};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Client, Data}, #state{c_sock=Client, s_sock=Server}=State) ->
    gen_tcp:send(Server, Data),
    inet:setopts(Client, [{active, once}]),
    {noreply, State};

handle_info({tcp, Server, Data}, #state{c_sock=Client, s_sock=Server}=State) ->
    inet:send(Client, Data),
    inet:setopts(Server, [{active, once}]),
    {noreply, State};

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _ ,_}, State) ->
    {stop, normal, State};

handle_info(timeout, #state{c_sock=Client, s_host=SHost, s_port=SPort}=State) ->
     case gen_tcp:connect(SHost, SPort, [binary, {packet, raw}, {active, once}]) of
        {ok, Server} ->
            inet:setopts(Client, [{active, once}, binary]),
            {noreply, State#state{s_sock=Server}};
        Error ->
            error_logger:error_msg("Failed to connect to server ip=~p port=~p error=~100p", [SHost, SPort, Error]),
            {stop, normal, State}
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

