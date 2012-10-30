%% Copyright 2012 Kevin A. Smith All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-module(hjalp).

-export([d/1,
         d/2,
         d/3,
         s/1,
         s/2,
         s/3]).

d(M, F) ->
    d({M, F}).
d(M, F, A) ->
    d({M, F, A}).
d(M) when is_atom(M) ->
    case load_attributes(M) of
        {ok, Attrs} ->
            display_help(Attrs, M);
        Error ->
            io:format("~p~n", [Error])
    end;
d({M, F}) ->
    case load_attributes(M) of
        {ok, Attrs} ->
            display_help(Attrs, M, F);
        Error ->
            io:format("~p~n", [Error])
    end;
d({M, F, A}) ->
    case load_attributes(M) of
        {ok, Attrs} ->
            display_help(Attrs, M, F, A);
        Error ->
            io:format("~p~n", [Error])
    end.

s(M, F) ->
    s({M, F}).
s(M, F, A) ->
    s({M, F, A}).
s(M) when is_atom(M) ->
    case load_attributes(M) of
        {ok, Attrs} ->
            display_specs(Attrs, M);
        Error ->
            io:format("~p~n", [Error])
    end;
s({M, F}) ->
    case load_attributes(M) of
        {ok, Attrs} ->
            display_specs(Attrs, M, F);
        Error ->
            io:format("~p~n", [Error])
    end;
s({M, F, A}) ->
    case load_attributes(M) of
        {ok, Attrs} ->
            display_specs(Attrs, M, F, A);
        Error ->
            io:format("~p~n", [Error])
    end.

%% Internal functions
display_help(Attrs, M) ->
    case get_docs(Attrs) of
        {ok, Hjalp} ->
            [print_help(M, Doc) || Doc <- Hjalp],
            ok;
        Error ->
            Error
    end.

display_help(Attrs, M, F) ->
    case get_docs(Attrs) of
        {ok, Hjalp} ->
            [print_help(M, Doc) || Doc <- Hjalp,
                                   case Doc of
                                       {{F, _}, _} ->
                                           true;
                                       _ ->
                                           false
                                   end],
            ok;
        Error ->
            Error
    end.

display_help(Attrs, M, F, A) ->
    case get_docs(Attrs) of
        {ok, Hjalp} ->
            case proplists:get_value({F, A}, Hjalp) of
                undefined ->
                    no_help;
                Doc ->
                    print_help(M, F, A, Doc),
                    ok
            end;
        Error ->
            Error
    end.

display_specs(Attrs, M) ->
    case get_specs(Attrs) of
        {ok, Hjalp} ->
            [print_specs(M, Doc) || Doc <- Hjalp],
            ok;
        Error ->
            Error
    end.

display_specs(Attrs, M, F) ->
    case get_specs(Attrs) of
        {ok, Hjalp} ->
            [print_specs(M, Doc) || Doc <- Hjalp,
                                   case Doc of
                                       {{F, _}, _} ->
                                           true;
                                       _ ->
                                           false
                                   end],
            ok;
        Error ->
            Error
    end.

display_specs(Attrs, M, F, A) ->
    case get_specs(Attrs) of
        {ok, Hjalp} ->
            case proplists:get_value({F, A}, Hjalp) of
                undefined ->
                    no_specs;
                Doc ->
                    print_specs(M, F, A, Doc),
                    ok
            end;
        Error ->
            Error
    end.


print_help(M, {{F, A}, Doc}) ->
    print_help(M, F, A, Doc).

print_help(_M, _F, _A, "") ->
    ok;
print_help(_M, F, A, Doc) ->
    io:format("~p/~p: ~p~n", [F, A, Doc]).

print_specs(M, {{F, A}, Doc}) ->
    print_specs(M, F, A, Doc).

print_specs(_M, _F, _A, "") ->
    ok;
print_specs(_M, _F, _A, Doc) ->
    io:format("~s~n", [Doc]).

get_docs(Attrs) ->
    case proplists:get_value(hjalp_docs, Attrs) of
        undefined ->
            not_hjalp_compiled;
        Hjalp ->
            {ok, Hjalp}
    end.

get_specs(Attrs) ->
    case proplists:get_value(hjalp_specs, Attrs) of
        undefined ->
            not_hjalp_compiled;
        Hjalp ->
            {ok, Hjalp}
    end.

load_attributes(M) ->
    case code:which(M) of
        non_existing ->
            non_existing;
        FileName0 ->
            FileName = filename:absname(FileName0),
            {ok, File} = file:read_file(FileName),
            {ok, {M, [{attributes, Attrs}]}} = beam_lib:chunks(File, [attributes]),
            {ok, Attrs}
    end.
