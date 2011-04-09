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
-module(rbuddy_tcp_listener).
-behaviour(gen_nb_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/2,
         sock_opts/0,
         new_connection/4,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {listener, slave, standby}).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    gen_nb_server:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([], State) ->
    Listener = get_listener(),
    MyState = #state{listener=Listener},
    {ok, gen_nb_server:store_cb_state(MyState, State), 0}.

sock_opts() ->
    [binary, {active, false}, {reuseaddr, true}, {nodelay, true}, {packet, raw}].

new_connection(_Host, _Port, Client, State) ->
    MyState = gen_nb_server:get_cb_state(State),
    Listener = MyState#state.listener,
    Slave = MyState#state.slave,
    Standby = MyState#state.standby,
    Master = get_master(),
    case rbuddy_tcp_proxy_sup:start_child(Client, Listener, Slave, Standby, Master) of
        {ok, _Pid} ->
            ok;
        Error ->
            error_logger:error_msg("Error creating TCP proxy: ~p~n", [Error]),
            gen_tcp:close(Client)
    end, 
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    MyState = gen_nb_server:get_cb_state(State),
    {Host, Port} = MyState#state.listener,
    case gen_nb_server:add_listen_socket({Host, Port}, State) of
        {ok, State1} ->
            erlang:send(self(), notify_slave),
            {noreply, State1};
        Error ->
            {stop, Error, State}
    end;

handle_info(notify_slave, State) ->
    MyState = gen_nb_server:get_cb_state(State),
    [{Host, Port}, Standby] = get_slaves(),
    case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}]) of
        {ok, Socket} ->
            {LHost, LPort} = MyState#state.listener,
            case rbuddy_redis:ok_cmd(Socket, rbuddy_redis_proto:slave_of(LHost, LPort)) of
                ok ->
                    gen_tcp:close(Socket),
                    MyState1 = MyState#state{slave={Host, Port}, standby=Standby},
                    State1 = gen_nb_server:store_cb_state(MyState1, State),
                    {noreply, State1};
                Error ->
                    {stop, Error, State}
            end;
        Error ->
            {stop, Error, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% internal functions
%%====================================================================
get_listener() ->
    case rbuddy_app:app_var(rbuddy) of
        {Host, Port} when is_list(Host), is_integer(Port) ->
            {Host, Port};
        Other ->
            error_logger:error_msg("Could not read rbuddy app var ~p~n", [Other]), 
            exit(invalid_app_var)
    end.

get_master() ->
    case rbuddy_app:app_var(master) of
        {Host, Port} when is_list(Host), is_integer(Port) ->
            {Host, Port};
        Other ->
            error_logger:error_msg("Could not read master app var ~p~n", [Other]), 
            exit(invalid_app_var)
    end.

get_slaves() ->
    case rbuddy_app:app_var(slaves) of
        [{H1,P1},{H2,P2}] when is_list(H1), is_integer(P1), is_list(H2), is_integer(P2) ->
            [{H1, P1}, {H2, P2}];
        Other ->
            error_logger:error_msg("Could not read slaves app var ~p~n", [Other]), 
            exit(invalid_app_var)
    end.
