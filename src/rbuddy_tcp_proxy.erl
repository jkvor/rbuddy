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
-export([start_link/1, attach/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {listener, s_sock,
                c_host, c_port, c_sock}).

%%====================================================================
%% API functions
%%====================================================================
start_link(Listener) ->
    gen_server:start_link(?MODULE, [Listener], []).

attach(Pid, Client) ->
    gen_server:cast(Pid, {attach, Client}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Listener]) ->
    State = #state{listener=Listener},
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, invalid_msg, State}.

handle_cast({attach, Client}, State) ->
    {CHost, CPort} = rbuddy_tcp_utils:peerinfo(Client),
    error_logger:info_msg("Accepted new connection ~p:~w~n", [CHost, CPort]),
    {noreply, State#state{c_host=CHost,
                          c_port=CPort,
                          c_sock=Client}, 0};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{c_sock=Client}=State) ->
    {SHost, SPort} = rbuddy:master(),
    case gen_tcp:connect(SHost, SPort, [binary, {packet, raw}, {active, once}]) of
        {ok, Server} ->
            error_logger:info_msg("Established connection to master ~s:~w~n", [SHost, SPort]),
            inet:setopts(Client, [{active, once}, binary]),
            {noreply, State#state{s_sock=Server}};
        Error ->
            error_logger:error_msg("Failed to connect to server ~p:~w ~100p~n", [SHost, SPort, Error]),
            {stop, normal, State}
    end;

handle_info({tcp, Client, Data}, #state{c_sock=Client, s_sock=Server}=State) ->
    gen_tcp:send(Server, Data),
    inet:setopts(Client, [{active, once}]),
    {noreply, State};

handle_info({tcp, Server, Data}, #state{c_sock=Client, s_sock=Server}=State) ->
    inet:send(Client, Data),
    inet:setopts(Server, [{active, once}]),
    {noreply, State};

handle_info({tcp_closed, Server}, #state{c_sock=Client, s_sock=Server}=State) ->
    case rbuddy:readonly_mode() of
        ok ->
            gen_tcp:close(Client),
            {stop, normal, State};
        Error ->
            {stop, Error, State}
    end;

handle_info({tcp_closed, Client}, #state{c_sock=Client}=State) ->
    {stop, normal, State};

handle_info({tcp_error, Server, Reason}, #state{c_sock=Client, s_sock=Server}=State) ->
    error_logger:error_report([tcp_error, server, Reason]),
    case rbuddy:readonly_mode() of
        ok ->
            gen_tcp:close(Client),
            {stop, normal, State};
        Error ->
            {stop, Error, State}
    end;

handle_info({tcp_error, Client, Reason}, #state{c_sock=Client}=State) ->
    error_logger:error_report([tcp_error, client, Reason]),
    {stop, normal, State};

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
