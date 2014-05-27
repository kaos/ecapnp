%%  
%%  Copyright 2013, Andreas Stenius <kaos@astekk.se>
%%  
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%  
%%     http://www.apache.org/licenses/LICENSE-2.0
%%  
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%  

%% @copyright 2013, Andreas Stenius
%% @author Andreas Stenius <kaos@astekk.se>
%% @doc The Erlang Cap'n Proto Compiler plugin.
%%
%% This module takes a <a
%% href="http://kentonv.github.io/capnproto/otherlang.html#how_to_write_compiler_plugins">`CodeGeneratorRequest'</a>
%% message and compiles the requested schema files into Erlang header
%% files.
%%
%% Thus, a `my_schema.capnp' will be compiled to
%% `my_schema.capnp.hrl', implementing `my_schema/N' functions for
%% reading and writing Cap'n Proto messages using `my_schema'.
%%
%%
%% == Schema functions ==
%%
%% The schema functions implemented in a compiled schema header file are:
%% <dl>
%%   <dt>{@type fun((root, schema_type(), message()) -> {ok, object()@})}</dt>
%%   <dd>Get a reference to the root object in message.</dd>
%%
%%   <dt>{@type fun((root, schema_type()) -> {ok, object()@})}</dt>
%%   <dd>Set root object type for a new message.</dd>
%%
%%   <dt>{@type fun((get, field_name(), object()) -> field_value())}</dt>
%%   <dd>Read object field value.</dd>
%%
%%   <dt>{@type fun((get, object()) -> (field_name() | {field_name(), field_value()@}))}</dt>
%%   <dd>Read unnamed union value of object.</dd>
%%
%%   <dt>{@type fun((set, field_name(), field_value(), object()) -> ok)}</dt>
%%   <dd>Write value to object field.</dd>
%%
%%   <dt>{@type fun((set, {field_name(), field_value()@}|field_name(), object()) -> ok)}</dt>
%%   <dd>Write unnamed union value.</dd>
%%
%%   <dt>{@type fun((to_struct, schema_type(), object()) -> object())}</dt>
%%   <dd>Type cast object to another struct type.</dd>
%%
%%   <dt>{@type fun((to_list, schema_type(), object()) -> list())}</dt>
%%   <dd>Type cast object to list.</dd>
%%
%%   <dt>{@type fun((to_text | to_data, object()) -> binary())}</dt>
%%   <dd>Type cast object to text or data.</dd>
%%
%%   <dt>{@type fun((schema) -> schema())}</dt>
%%   <dd>Get the compiled schema definition.</dd>
%% </dl>
%%
%% Where `fun' is named after the basename of the schema file (refer
%% to the `my_schema.capnp' example in the {@section Description} section).

-module(ecapnpc).
-author("Andreas Stenius <kaos@astekk.se>").

-export([compile_file/1,
         compile_data/1,
         compile_message/1
        ]).

-include("ecapnp.hrl").

%% TODO: add options argument, so os environment only acts as override..

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Read a `CodeGeneratorRequest' message (unpacked) from
%% `FileName' and compile it.
-spec compile_file( file:name_all() ) -> ok.
compile_file(FileName) ->
    {ok, Data} = file:read_file(FileName),
    compile_data(Data).

%% @doc Compile the `CodeGeneratorRequest' message (unpacked) in
%% `Data'.
-spec compile_data( binary() ) -> ok.
compile_data(Data)
  when is_binary(Data) ->
    {ok, Message, <<>>} = ecapnp_message:read(Data),
    compile_message(Message).

%% @doc Compile the `CodeGeneratorRequest' message. The `Message'
%% argument holds the raw segments data to process, no futher
%% processing on the message itself will be carried out prior to the
%% compilation step.
-spec compile_message( message() ) -> ok.
compile_message(Message)
  when is_list(Message), is_binary(hd(Message)) ->
    {ok, Compiled} = ecapnp_compiler:compile(Message),
    ok = maybe_save_sources(Compiled),
    ok = maybe_compile_ast(Compiled).


%% ===================================================================
%% Internal functions
%% ===================================================================

maybe_save_sources(Compiled) ->
    case os:getenv("ECAPNP_TO_ERL") of
        false -> ok;
        "" -> save_sources(Compiled, ".");
        Path -> save_sources(Compiled, Path)
    end.

maybe_compile_ast(Compiled) ->
    case os:getenv("ECAPNP_NO_BEAM") of
        false -> compile_ast(Compiled);
        _ -> ok
    end.

save_sources([], _Path) -> ok;
save_sources([{File, Ast}|Compiled], Path) ->
    Filename = filename:join(Path, <<File/binary, ".erl">>),
    case write_file(
           Filename,
           erl_prettypr:format(Ast, [{ribbon, 100}, {paper, 200}]))
    of
        ok -> save_sources(Compiled, Path);
        error -> halt(2)
    end.

compile_ast([]) -> ok;
compile_ast([{_File, Ast}|Compiled]) ->
    Forms = erl_syntax:revert_forms(Ast),
    case compile:forms(
           Forms,
           [verbose, report, debug_info]) of
        {ok, Module, Bin} ->
            Filename = lists:concat([Module, ".beam"]),
            case write_file(Filename, Bin) of
                ok -> maybe_load(Module, Filename, Bin);
                error -> halt(2)
            end,
            compile_ast(Compiled);
        error -> halt(1)
    end.

maybe_load(Module, Filename, Bin) ->
    case os:getenv("ECAPNP_LOAD_BEAM") of
        false -> ok;
        _ ->
            case code:load_binary(Module, Filename, Bin) of
                {module, Module} -> ok;
                {error, Reason} ->
                    io:format(standard_error,
                              "~s: warning: failed to load: ~p~n",
                              [Filename, Reason])
            end
    end.

write_file(Filename, Data) ->
    case file:write_file(Filename, Data) of
        ok -> ok;
        {error, Reason} ->
            io:format(standard_error,
                     "~s: failed to write file: ~p~n",
                      [Filename, Reason]),
            error
    end.
