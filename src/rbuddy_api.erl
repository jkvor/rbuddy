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
-module(rbuddy_api).
-export([start_link/0, loop/1]).

-define(HDR, [{"Content-Type", "text/html"}]).

start_link() ->
    {Host, Port} = get_api(),    
    Opts = [
        {ip, Host},
        {port, Port},
        {backlog, 1024},
        {loop, {?MODULE, loop}}
    ],
    mochiweb_http:start(Opts).

loop(Req) ->
    Method = Req:get(method),
    Path = Req:get(path),
    case [Method, Path] of
        ['GET', "/active"] ->
            active(Req);
        ['GET', "/standby"] ->
            standby(Req);
        _ ->
            Req:respond({404, ?HDR, <<>>})
    end.

active(Req) ->
    case rbuddy_tcp_listener:active_slave() of
        {Host, Port} when is_list(Host), is_integer(Port) ->
            Req:respond({200, ?HDR, ["redis://", Host, ":", integer_to_list(Port), "/"]});
        _ ->
            Req:respond({500, ?HDR, <<>>})
    end.

standby(Req) ->
    case rbuddy_tcp_listener:standby_slave() of
        {Host, Port} when is_list(Host), is_integer(Port) ->
            Req:respond({200, ?HDR, ["redis://", Host, ":", integer_to_list(Port), "/"]});
        _ ->
            Req:respond({500, ?HDR, <<>>})
    end.

get_api() ->
    case rbuddy_app:app_var(rbuddy_api) of
        {Host, Port} when is_list(Host), is_integer(Port) ->
            {Host, Port};
        Other ->
            error_logger:error_msg("Could not read rbuddy_api app var ~p~n", [Other]), 
            exit(invalid_app_var)
    end.
