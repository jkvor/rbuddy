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
-module(rbuddy_redis).
-export([ok_cmd/2, info/1]).

-define(NL, <<"\r\n">>).

ok_cmd(Sock, Cmd) ->
    case gen_tcp:send(Sock, Cmd) of
        ok ->
            case gen_tcp:recv(Sock, 0) of
                {ok, <<"+OK\r\n">>} ->
                    ok;
                Error ->
                    error_logger:error_report([?MODULE, ?LINE, Error]),
                    Error
            end;
        Error ->
            error_logger:error_report([?MODULE, ?LINE, Error]),
            Error
    end.

info(Sock) ->
    case gen_tcp:send(Sock, rbuddy_redis_proto:info()) of
        ok ->
            inet:setopts(Sock, [{packet, line}]),
            case gen_tcp:recv(Sock, 0) of
                {ok, <<"$", Size/binary>>} ->
                    Size1 = list_to_integer(binary_to_list(strip_nl(Size))),
                    inet:setopts(Sock, [{packet, raw}]), 
                    case gen_tcp:recv(Sock, Size1+2) of
                       {ok, BinInfo} ->
                           [list_to_tuple(string:tokens(Item, ":")) || Item <- string:tokens(binary_to_list(BinInfo), "\r\n")];
                       Error ->
                           error_logger:error_report([?MODULE, ?LINE, Error]),
                           Error
                    end;
                {ok, Other} ->
                    error_logger:error_report([?MODULE, ?LINE, {error, Other}]),
                    {error, Other};
                Error ->
                    error_logger:error_report([?MODULE, ?LINE, Error]),
                    Error
            end;
        Error ->
            error_logger:error_report([?MODULE, ?LINE, Error]),
            Error
    end.

strip_nl(B) when is_binary(B) ->
    S = size(B) - size(?NL),
    <<B1:S/binary, _/binary>> = B,
    B1.
