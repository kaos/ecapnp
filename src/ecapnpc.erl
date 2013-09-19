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

-export([compile_file/1,
         compile_data/1,
         compile_message/1]).

-include_lib("ecapnp/include/schema.capnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

compile_file(FileName) ->
    {ok, Data} = file:read_file(FileName),
    compile_data(Data).

compile_data(Data)
  when is_binary(Data) ->
    {ok, Message} = ecapnp_message:read(Data),
    compile_message(Message).

compile_message(Message)
  when is_list(Message), is_binary(hd(Message)) ->
    {ok, Root} = schema(root, 'CodeGeneratorRequest', Message),
    Compiled = compile_root(Root),
    export(Compiled).

%% ===================================================================
%% internal functions
%% ===================================================================

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
        ok
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
         "  #schema{ %% 0x~.16b~n"
         "    name=~p, id=~b, source= ~p",
         Name, Id, Name, Id, Src]),
    export_list("types", Types, Out, 4),
    Out(["}.~n"]).

export_list(_Label, [], _Out, _Indent) -> ok;
export_list(Label, List, Out, Indent) ->
    I = Indent + 2,
    Out([",~n~*s~s=~n~*s[", Indent, "", Label, I, ""]),
    export_items(List, Out, {first, I + 1}),
    Out(["~n~*s]", I, ""]).

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
                     dsize=DSize, psize=PSize, esize=ESize,
                     fields=Fields, types=Types, union_field=Union },
           Out, Indent) ->
    I = Indent + 2,
    Out(["#struct{ %% 0x~.16b~n~*s"
         "name=~p, id=~b, source= ~p,~n~*s"
         "dsize=~p, psize=~p, esize=~p,~n~*s"
         "union_field=",
         Id, I, "", Name, Id, Src,
         I, "", DSize, PSize, ESize,
         I, ""]),
    export_item(Union, Out, I),
    export_list("fields", Fields, Out, I),
    export_list("types", Types, Out, I),
    Out(["}"]);
export_item(#enum{ id=Id, source=Src, name=Name,
                   values=Values, types=Types },
           Out, Indent) ->
    I = Indent + 2,
    Out(["#enum{ %% 0x~.16b~n~*s"
         "name=~p, id=~b, source= ~p",
         Id, I, "", Name, Id, Src]),
    export_list("values", Values, Out, I),
    export_list("types", Types, Out, I),
    Out(["}"]);
export_item(#data{ type={union, L}, align=A }, Out, Indent) ->
    I = Indent + 2,
    Out(["#data{ align=~p, type=~n~*s"
         "{union,~n~*s[",
         A, I, "", I + 2, ""]),
    export_items(L, Out, {first, I + 3}),
    Out(["~n~*s]} }", I, ""]);
export_item(#data{ type=T, align=A }, Out, _Indent) ->
    Out(["#data{ type=~p, align=~p }", T, A]);
export_item(#ptr{ type=T, idx=I }, Out, _Indent) ->
    Out(["#ptr{ type=~p, idx=~p }", T, I]);
export_item(#group{ id=Id }, Out, _Indent) ->
    Out(["#group{ id=~p }", Id]);
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
    [link_node(schema(get, id, File), root, Nodes) || File <- Files].

compile_node(Node) ->
    {schema(get, id, Node),
     {Node, compile_node(
              schema(get, Node),
              Node)}
    }.

compile_node(file, Node) ->
    #schema{ id = schema(get, id, Node), source = schema(get, displayName, Node) };
compile_node({struct, Struct}, Node) ->
    {Fields, Union} = lists:partition(
                        fun (F) ->
                                0 == schema(get, discriminantValue, F)
                        end,
                        schema(get, fields, Struct)),
    #struct{ id = schema(get, id, Node),
             source = schema(get, displayName, Node),
             dsize = schema(get, dataWordCount, Struct),
             psize = schema(get, pointerCount, Struct),
             esize = schema(get, preferredListEncoding, Struct),
             fields = [compile_struct_field(M)
                       || M <- Fields],
             union_field = case schema(get, discriminantCount, Struct) of
                               0 -> none;
                               _ -> data_field(
                                      {{union,
                                        [compile_struct_field(F) || F <- Union]}, 16},
                                      schema(get, discriminantOffset, Struct))
                           end
           };
compile_node({enum, Enum}, Node) ->
    #enum{ id = schema(get, id, Node),
           source = schema(get, displayName, Node),
           values =
               [binary_to_atom(schema(get, name, E), latin1) 
                || E <- schema(get, enumerants, Enum)]
         };
compile_node(_, _) ->
    skip.

compile_struct_field(Field) ->
    {binary_to_atom(schema(get, name, Field), latin1),
     compile_struct_field_type(schema(get, Field))}.

compile_struct_field_type({slot, Field}) ->
    Type = schema(get, type, Field),
    compile_field(schema(get, Type), schema(get, offset, Field));
compile_struct_field_type({group, Id}) ->
    #group{ id=Id }.

compile_field({list, Type}, Offset) -> ptr_field(list_field_type(Type), Offset);
compile_field({enum, Id}, Offset) -> data_field({{enum, Id}, 16}, Offset);
compile_field({struct, Id}, Offset) -> ptr_field({struct, Id}, Offset);
compile_field({interface, Id}, Offset) -> ptr_field({interface, Id}, Offset);
compile_field(object, Offset) -> ptr_field(object, Offset);
compile_field(text, Offset) -> ptr_field(text, Offset);
compile_field(Type, Offset)
  when is_atom(Type) ->
    data_field(capnp_type_info(Type), Offset).

list_field_type(List) ->
    Type = schema(get, elementType, List),
    {list, 
     case schema(get, Type) of
         void -> void;
         CapnpType when is_atom(CapnpType) ->
             {DataType, _} = capnp_type_info(CapnpType),
             DataType;
         {list, ListType} ->
             list_field_type(ListType);
         {Kind, Id} -> 
             {Kind, Id}
     end}.

capnp_type_info(void) -> void;
capnp_type_info(bool) -> {bool, 1};
capnp_type_info(int8) -> {int8, 8};
capnp_type_info(int16) -> {int16, 16};
capnp_type_info(int32) -> {int32, 32};
capnp_type_info(int64) -> {int64, 64};
capnp_type_info(uint8) -> {uint8, 8};
capnp_type_info(uint16) -> {uint16, 16};
capnp_type_info(uint32) -> {uint32, 32};
capnp_type_info(uint64) -> {uint64, 64};
capnp_type_info(float32) -> {float32, 32};
capnp_type_info(float64) -> {float64, 64};
capnp_type_info(text) -> {text, 8};
capnp_type_info(data) -> {data, 8};
capnp_type_info(Other) -> throw({unknown_capnp_type, Other}).
    
data_field(void, _Offset) -> void;
data_field({Type, 1}, Offset) ->
    %% compensate for erlangs big endian bit streams.. yuck!
    data_field({Type, 8}, (Offset div 8) + ((7 - (Offset rem 8))/8));
data_field({Type, Size}, Offset) ->
    %% convert alignment to integer (Size * Offset should never be a fraction)
    #data{ type=Type, align=round(Size * Offset) }.

ptr_field(Type, Index) ->
    #ptr{ type=Type, idx=Index }.


link_node(Id, NodeName, Nodes) ->
    {Node, Schema} = proplists:get_value(Id, Nodes),
    NestedNodes = [begin
                       Name = binary_to_atom(schema(get, name, N), latin1),
                       link_node(schema(get, id, N), Name, Nodes)
                   end || N <- schema(get, nestedNodes, Node)],
    Groups = case schema(get, Node) of
                 {struct, S} ->
                     lists:foldl(
                       fun(F, G) ->
                               case schema(get, F) of
                                   {group, GroupId} ->
                                       Name = binary_to_atom(schema(get, name, F), latin1),
                                       [link_node(GroupId, Name, Nodes)|G];
                                   _ -> G
                               end
                       end,
                       [],
                       schema(get, fields, S));
                 _ -> []
             end,
    Types = NestedNodes ++ Groups,
    set_types(Schema, NodeName, Types).

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
