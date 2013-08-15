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

-module(ecapnpc).
-author("Andreas Stenius <kaos@astekk.se>").

-export([file/1]).

-include("schema.capnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

file(FileName) ->
    {ok, Data} = file:read_file(FileName),
    {ok, Message} = ecapnp_message:read(Data),
    compile(Message).


%% ===================================================================
%% internal functions
%% ===================================================================

compile(Msg) ->
    {ok, Root} = schema(root, 'CodeGeneratorRequest', Msg),
    Compiled = compile_root(Root),
    export(Compiled).

export([Schema|Ss]) ->
    case export_schema(Schema) of
        ok -> export(Ss);
        Err -> Err
    end;
export([]) -> ok.

export_schema(#schema{ source=Src }=Schema) ->
    FileName = get_output_filename(Src),
    {ok, File} = create_output_file(FileName),
    try 
        Out = fun ([Fmt|Args]) ->
                      ok = io:format(File, Fmt, Args) 
              end,
        [Write(Out, Schema) 
         || Write <- [fun write_header/2,
                      fun write_api/2,
                      fun export_schema/2
                     ]
        ],
        io:format("compiled ~s~n", [FileName])
    after
        file:close(File)
    end.

write_header(Out, _) ->
    Out(["%%% DO NOT EDIT, this file was generated.~n"
         "-include_lib(\"ecapnp/include/ecapnp.hrl\").~n"]).

write_api(Out, #schema{ name=Name }) ->
    {ok, Stubs} = file:read_file(
                    filename:join(
                      code:priv_dir(ecapnp), 
                      "schema_api_stub.hrl")),
    Api = lists:foldl(
            fun ({Re, R}, Subject) ->
                    re:replace(Subject, Re, R, [global])
            end, Stubs, 
            [{"_name_", atom_to_list(Name)}
            ]),
    Out(["~s~n", Api]).

export_schema(Out, #schema{ id=Id, source=Src, name=Name, types=Types }) ->
    Out(["~p(schema) ->~n"
         "  #schema{~n"
         "    name=~p, id=~.16+, source= ~p,~n"
         "    types=~n",
         Name, Name, Id, Src]),
    export_list(Types, Out, 6),
    Out(["}.~n"]).

export_list(List, Out, Indent) ->
    Out(["~*s[", Indent, ""]),
    export_items(List, Out, {first, Indent + 1}),
    Out(["~n~*s]", Indent, ""]).

export_items([Item|List], Out, {first, Indent}) ->
    export_item(Item, Out, Indent),
    export_items(List, Out, Indent);
export_items([Item|List], Out, Indent) ->
    Out([",~n~*s", Indent, ""]),
    export_item(Item, Out, Indent),
    export_items(List, Out, Indent);
export_items([], _, _) -> ok.

export_item({Tag, Type}, Out, Indent) 
  when is_tuple(Type), is_atom(element(1, Type)) ->
    I = Indent + 1,
    Out(["{~p,~n~*s", Tag, I, ""]),
    export_item(Type, Out, I),
    Out(["}"]);
export_item(#struct{ id=Id, source=Src, name=Name,
                     dsize=DSize, psize=PSize,
                     fields=Fields, types=Types },
           Out, Indent) ->
    I = Indent + 2,
    Out(["#struct{~n~*s"
         "name=~p, id=~.16+, source= ~p,~n~*s"
         "dsize=~p, psize=~p, fields=~n",
         I, "", Name, Id, Src,
         I, "", DSize, PSize]),
    export_list(Fields, Out, I + 2),
    if Types /= [] ->
            Out([",~n~*s"
                 "types=~n",
                 I, ""]),
            export_list(Types, Out, I + 2);
       true -> nop
    end,
    Out(["}"]);
export_item(#enum{ id=Id, source=Src, name=Name,
                   values=Values, types=Types },
           Out, Indent) ->
    I = Indent + 2,
    Out(["#enum{~n~*s"
         "name=~p, id=~.16+, source= ~p,~n~*s"
         "values=~n",
         I, "", Name, Id, Src, I, ""]),
    export_list(Values, Out, I + 2),
    if Types /= [] ->
            Out([",~n~*s"
                 "types=~n",
                 I, ""]),
            export_list(Types, Out, I + 2);
       true -> nop
    end,
    Out(["}"]);
export_item(#data{ type={union, L}, align=A }, Out, Indent) ->
    I = Indent + 2,
    Out(["#data{ align=~p, type=~n~*s"
         "{union,~n",
         A, I, ""]),
    export_list(L, Out, I + 2),
    Out(["} }"]);
export_item(#data{ type=T, align=A }, Out, _Indent) ->
    Out(["#data{ type=~p, align=~p }", T, A]);
export_item(#ptr{ type=T, idx=I }, Out, _Indent) ->
    Out(["#ptr{ type=~p, idx=~p }", T, I]);
export_item(Item, Out, _Indent) ->
    Out(["~p", Item]).

get_output_filename(Src)
  when is_binary(Src) ->
    <<Src/binary, ".hrl">>.

create_output_file(FileName) ->
    ok = filelib:ensure_dir(FileName),
    file:open(FileName, [write]).

compile_root(Root) ->
    Nodes = [compile_node(N) || N <- schema(get, nodes, Root)],
    Files = schema(get, requestedFiles, Root),
    Linked = [link_node(Id, root, Nodes) || Id <- Files],
    IdNames = lists:flatten([collect_names(Schema, []) || Schema <- Linked]),
    [resolve_names(Schema, IdNames) || Schema <- Linked].

compile_node(Node) ->
    Body = schema(get, body, Node),
    {schema(get, id, Node), {Node, compile_body(Body, Node)}}.

compile_body({fileNode, _}, Node) ->
    #schema{ id = schema(get, id, Node), source = schema(get, displayName, Node) };
compile_body({structNode, Struct}, Node) ->
    #struct{ id = schema(get, id, Node),
             source = schema(get, displayName, Node),
             dsize = schema(get, dataSectionWordSize, Struct),
             psize = schema(get, pointerSectionSize, Struct),
             fields=
                 [compile_struct_member(M)
                  || M <- schema(get, members, Struct)]};
compile_body({enumNode, Enum}, Node) ->
    #enum{ id = schema(get, id, Node),
           source = schema(get, displayName, Node),
           values =
               [binary_to_atom(schema(get, name, E), latin1) 
                || E <- schema(get, enumerants, Enum)]
         };
compile_body(_, _) ->
    skip.

compile_struct_member(Member) ->    
    {binary_to_atom(schema(get, name, Member), latin1),
     compile_struct_member_body(schema(get, body, Member))}.

compile_struct_member_body({fieldMember, Field}) ->
    Type = schema(get, type, Field),
    compile_field(schema(get, body, Type), schema(get, offset, Field));
compile_struct_member_body({unionMember, Union}) ->
    data_field({compile_union(Union), 16}, schema(get, discriminantOffset, Union));
compile_struct_member_body(_) ->
    this_struct_body_NYI.

compile_union(Union) ->
    {union, [compile_struct_member(M) || M <- schema(get, members, Union)]}.

compile_field({listType, Type}, Offset) -> ptr_field(list_field_type(Type), Offset);
compile_field({enumType, Id}, Offset) -> data_field({{enum, id_to_typename(Id)}, 16}, Offset);
compile_field({structType, Id}, Offset) -> ptr_field({struct, id_to_typename(Id)}, Offset);
compile_field({interfaceType, Id}, Offset) -> ptr_field({interface, id_to_typename(Id)}, Offset);
compile_field(textType, Offset) -> ptr_field(text, Offset);
compile_field(Type, Offset)
  when is_atom(Type) ->
    data_field(capnp_type_info(Type), Offset).

id_to_typename(Id) ->
    fun(IdNames) -> proplists:get_value(Id, IdNames, {unknown_id, Id}) end.
    
list_field_type(Type) ->
    {list, 
     case schema(get, body, Type) of
         voidType -> void;
         CapnpType when is_atom(CapnpType) ->
             {DataType, _} = capnp_type_info(CapnpType),
             DataType;
         {listType, ListType} ->
             list_field_type(ListType);
         {Kind, Id} ->
             {case Kind of
                  enumType -> enum;
                  structType -> struct;
                  interfaceType -> interface
              end,
              id_to_typename(Id)}
     end}.

capnp_type_info(voidType) -> void;
capnp_type_info(boolType) -> {bool, 1};
capnp_type_info(int8Type) -> {int8, 8};
capnp_type_info(int16Type) -> {int16, 16};
capnp_type_info(int32Type) -> {int32, 32};
capnp_type_info(int64Type) -> {int64, 64};
capnp_type_info(uint8Type) -> {uint8, 8};
capnp_type_info(uint16Type) -> {uint16, 16};
capnp_type_info(uint32Type) -> {uint32, 32};
capnp_type_info(uint64Type) -> {uint64, 64};
capnp_type_info(float32Type) -> {float32, 32};
capnp_type_info(float64Type) -> {float64, 64};
capnp_type_info(textType) -> {text, 8};
capnp_type_info(dataType) -> {data, 8};
capnp_type_info(objectType) -> {object, 8}. %% ???
    
data_field(void, _Offset) -> void;
data_field({Type, Size}, Offset) ->
    #data{ type=Type, align=Size * Offset }.

ptr_field(Type, Index) ->
    #ptr{ type=Type, idx=Index }.


link_node(Id, NodeName, Nodes) ->
    {Node, Schema} = proplists:get_value(Id, Nodes),
    NestedNodes = [begin
                       Name = binary_to_atom(schema(get, name, N), latin1),
                       {Name, link_node(schema(get, id, N), Name, Nodes)}
                   end || N <- schema(get, nestedNodes, Node)],
    set_types(Schema, NodeName, NestedNodes).

set_types(Node, root, Types)
  when is_record(Node, schema) ->
    Node#schema{ name=schema_name(Node), types=Types };
set_types(Node, Name, Types)
  when is_record(Node, struct) ->
    Node#struct{ name=Name, types=Types };
set_types(Node, Name, Types)
  when is_record(Node, enum) ->
    Node#enum{ name=Name, types=Types };
set_types(skip, _, _) ->
    [].

schema_name(#schema{ source=Src }) ->
    binary_to_atom(filename:basename(Src, ".capnp"), latin1).

collect_names(#schema{ types=Ts }, IdNames) ->
    IdNames ++ [collect_names(T, IdNames) || T <- Ts];
collect_names({_, #struct{ id=Id, name=Name, types=Ts }}, IdNames) ->
    [{Id, Name}|IdNames] ++ [collect_names(T, IdNames) || T <- Ts];
collect_names({_, #enum{ id=Id, name=Name, types=Ts }}, IdNames) ->
    [{Id, Name}|IdNames] ++ [collect_names(T, IdNames) || T <- Ts];
collect_names(_, IdNames) -> IdNames.

resolve_names(#schema{ types=Ts }=S, IdNames) ->
    S#schema{ types=[resolve_names(T, IdNames) || T <- Ts] };
resolve_names({Tag, #struct{ fields=Fs, types=Ts }=S}, IdNames) ->
    {Tag, S#struct{ fields=[resolve_names(F, IdNames) || F <- Fs],
                    types=[resolve_names(T, IdNames) || T <- Ts]}};
resolve_names({Tag, #enum{ types=Ts }=E}, IdNames) ->
    {Tag, E#enum{ types=[resolve_names(T, IdNames) || T <- Ts]}};
resolve_names({Tag, #data{ type=T }=D}, IdNames) ->
    {Tag, D#data{ type=resolve_names(T, IdNames) }};
resolve_names({Tag, #ptr{ type=T }=P}, IdNames) ->
    {Tag, P#ptr{ type=resolve_names(T, IdNames) }};
resolve_names({union, List}, IdNames) ->
    {union, [resolve_names(U, IdNames) || U <- List]};
resolve_names({list, Type}, IdNames) ->
    {list, resolve_names(Type, IdNames)};
resolve_names({Tag, Fun}, IdNames)
  when is_function(Fun, 1) ->
    {Tag, Fun(IdNames)};
resolve_names(Pass, _) -> Pass.
