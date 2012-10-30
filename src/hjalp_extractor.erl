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

-module(hjalp_extractor).

-include_lib("xmerl/include/xmerl.hrl").

-export([extract_fdoc/1,
         extract_fspecs/2]).

extract_fdoc(FileName) when is_list(FileName) ->
    Env = edoc_lib:get_doc_env([], [], [], []),
    compile_docs(edoc_extract:source(FileName, Env, [])).

extract_fspecs(Forms, Options) when is_list(Forms) ->
    parse_trans:inspect(fun compile_specs/4, [], Forms, Options).

%% Internal functions
compile_specs(attribute, {attribute, _Line, spec, {FunDesc, TypeDesc}}, _Context, Acc) ->
    {false, [build_spec(FunDesc, TypeDesc) | Acc]};
compile_specs(_Type, _Form, _Context, Acc) ->
    {false, Acc}.

compile_docs({_Module, #xmlElement{name=module, content=Content}}) ->
    compile_docs(Content, []).

compile_docs([], Docs) ->
    Docs;
compile_docs([#xmlElement{name=functions, content=Content}|_], Docs) ->
    compile_docs(Content, Docs);
compile_docs([#xmlElement{name=function, attributes=Attrs, content=Content}|T], Docs) ->
    {Name, Arity} = function_info(Attrs),
    Desc = function_description(Content),
    compile_docs(T, [{{Name, Arity}, Desc}|Docs]);
compile_docs([_|T], Docs) ->
    compile_docs(T, Docs).

function_info([#xmlAttribute{name=name, value=Name},
               #xmlAttribute{name=arity, value=Arity0}|_]) ->
    {list_to_atom(Name), list_to_integer(Arity0)}.

function_description([]) ->
    "";
function_description([#xmlElement{name=description, content=BriefAndFull}|_]) ->
    function_description1(BriefAndFull);
function_description([_|T]) ->
    function_description(T).

function_description1([]) ->
    "";
function_description1([#xmlElement{name=fullDescription, content=[#xmlText{value=Desc}]}|_]) ->
    Desc;
function_description1([_|T]) ->
    function_description1(T).

build_spec({FunName, _}=FunKey, [{type, _Line, 'fun', [Args, Result]}]) ->
    ArgText = translate_types(Args),
    ResultText = translate_types(Result),
    Text = lists:flatten(atom_to_list(FunName) ++ "(" ++ ArgText ++ ") -> " ++ ResultText),
    {FunKey, Text}.

%% Function args
translate_types({type, _Line, product, []}) ->
    "";
translate_types({type, _Line, product, Args}) ->
    string:join([translate_type(Arg) || Arg <- Args], ", ");
translate_types(TypeSpec) when is_tuple(TypeSpec) ->
    translate_type(TypeSpec);
translate_types(TypeSpecs) when is_list(TypeSpecs) ->
    string:join([translate_type(TypeSpec) || TypeSpec <- TypeSpecs], ", ").


translate_type({atom, _Line, Value}) ->
    atom_to_list(Value);
translate_type({type, _Line, Type, []}) ->
    atom_to_list(Type) ++ "()";
translate_type({type, _Line, list, [MemberType]}) ->
    "[" ++ translate_type(MemberType) ++ "]";
translate_type({type, _Line, tuple, Members}) ->
    Members1 = string:join([translate_type(Member) || Member <- Members], ", "),
    "{" ++ Members1 ++ "}";
translate_type({type, _Line, union, Types}) ->
    string:join([translate_type(Type) || Type <- Types], " | ").
