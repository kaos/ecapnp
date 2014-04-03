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
    {ok, Message} = ecapnp_message:read(Data),
    compile_message(Message).

%% @doc Compile the `CodeGeneratorRequest' message. The `Message'
%% argument holds the raw segments data to process, no futher
%% processing on the message itself will be carried out prior to the
%% compilation step.
-spec compile_message( message() ) -> ok.
compile_message(Message)
  when is_list(Message), is_binary(hd(Message)) ->
    {ok, Compiled} = ecapnp_compiler:compile(Message),
    %% Save to .erl sources, for now..
    [begin
         Filename = <<File/binary, ".erl">>,
         {Filename,
          file:write_file(
            Filename,
            erl_prettypr:format(
              Ast, [{ribbon, 100}, {paper, 200}]))}
     end || {File, Ast} <- Compiled].


%% ===================================================================
%% Internal functions
%% ===================================================================
