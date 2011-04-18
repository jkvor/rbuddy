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
-export([start_link/0, master/0, readonly_mode/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {listener, slave, master}).

-define(TIMEOUT, 10000).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

master() ->
    gen_server:call(?MODULE, master, ?TIMEOUT).

readonly_mode() ->
    gen_server:call(?MODULE, readonly_mode, ?TIMEOUT).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    Slave = get_slave(),
    Master = get_master(),
    Listener = get_listener(),
    {ok, #state{listener=Listener,
                slave=Slave,
                master=Master}}.

handle_call(master, _From, #state{master=Master}=State) ->
    {reply, Master, State};

handle_call(readonly_mode, _From, #state{slave=Slave}=State) ->
    case send_cmd(Slave, rbuddy_redis_proto:slave_of("NO", "ONE")) of
        ok ->
            {reply, ok, State};
        Error ->
            {stop, Error, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, invalid_msg, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% internal functions
%%====================================================================
get_slave() ->
    case rbuddy_app:app_var(slave) of
        {Host, Port} when is_list(Host), is_integer(Port) ->
            {Host, Port};
        Other ->
            error_logger:error_msg("Could not read slave app var ~p~n", [Other]), 
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

get_listener() ->
    case rbuddy_app:app_var(listener) of
        {Host, Port} when is_list(Host), is_integer(Port) ->
            {Host, Port};
        Other ->
            error_logger:error_msg("Could not read rbuddy app var ~p~n", [Other]), 
            exit(invalid_app_var)
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
