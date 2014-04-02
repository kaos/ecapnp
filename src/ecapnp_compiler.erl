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
%% @doc The Erlang Cap'n Proto Compiler.
%%

-module(ecapnp_compiler).
-author("Andreas Stenius <kaos@astekk.se>").

-export([compile/1]).

-import(erl_syntax, [atom/1, function/2, clause/2, record_expr/2,
                     clause/3, record_field/2, binary/1, underscore/0,
                     binary_field/1, application/2, form_list/1,
                     set_precomments/2, comment/1, string/1,
                     integer/1, tuple/1, arity_qualifier/2,
                     binary_field/3, list/1, attribute/2]).

-include("capnp/schema.capnp.hrl").

-type compiled_message() :: {file, erl_syntax:syntaxTree()}.

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Compile a `CodeGeneratorRequest' message.
-spec compile( message() ) -> {ok, [compiled_message()]}.
compile(Message)
  when is_list(Message), is_binary(hd(Message)) ->
    {ok, Root} = schema(root, 'CodeGeneratorRequest', Message),
    compile_root(Root).


%% ===================================================================
%% internal functions
%% ===================================================================

compile_root(Root) ->
    Nodes = schema(get, nodes, Root),
    Files = schema(get, requestedFiles, Root),
    {ok, [compile_file(File, Nodes) || File <- Files]}.

compile_file(File, Nodes) ->
    Id = schema(get, id, File),
    ExportedNodes = get_exported_nodes(Id, Nodes),
    Filename = compile_filename(File),

    Vsn =
        case application:get_key(ecapnp, vsn) of
            undefined ->
                ok = application:load(ecapnp),
                {ok, V} = application:get_key(ecapnp, vsn),
                V;
            {ok, V} -> V
        end,
    {{Y,M,D},{H,Mm,S}} = calendar:universal_time(),

    {Filename,
     form_list(
       [set_precomments(
          attribute(atom(module), [compile_modulename(Filename)]),
          [comment(
             [io_lib:format(
                "% This file was generated "
                "~4b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b"
                " UTC by ecapnp ~s.",
                [Y, M, D, H, Mm, S, Vsn]),
              "% http://github.com/kaos/ecapnp"])
          ]),
        attribute(atom(vsn), [integer(Id)]),
        attribute(atom(export), [compile_exports(ExportedNodes)]),
        attribute(atom(types), [compile_types(ExportedNodes)]),
        attribute(atom(include_lib), [string("ecapnp/include/ecapnp.hrl")])
        |compile_nodes(ExportedNodes)
       ])}.

compile_filename(File) ->
    B = binary:replace(
          filename:rootname(schema(get, filename, File), <<".capnp">>),
          <<".">>, <<"_">>, [global]),
    <<B/binary, "_capnp">>.
    
compile_modulename(Filename) ->
    atom(binary_to_list(filename:basename(Filename))).

compile_exports(ExportedNodes) ->
    Exports = lists:foldr(
                fun ({IdAst, NameAst, _Schema}, Acc) ->
                        [arity_qualifier(NameAst, integer(0)),
                         arity_qualifier(IdAst, integer(0))|Acc]
                end, [], ExportedNodes),
    list([arity_qualifier(atom(schema), integer(1))|Exports]).

compile_types(ExportedNodes) ->
    Types = [begin
                 Id = schema(get, id, Schema),
                 tuple([integer(Id), NameAst])
             end || {_IdAst, NameAst, Schema} <- ExportedNodes],
    list(Types).

compile_nodes(ExportedNodes) ->
    lists:foldr(
      fun ({IdAst, NameAst, _Schema}=Node, Acc) ->
              [function(
                 NameAst,
                 [clause(none, [application(IdAst, [])]) ]),
               compile_node(Node)
               |Acc]
      end,
      [function(
         atom(schema),
         lists:foldr(
           fun ({IdAst, NameAst, Schema}, Acc) ->
                   Id = schema(get, id, Schema),
                   [clause(
                      [NameAst],
                      none,
                      [application(atom(schema), [integer(Id)])]),
                    clause(
                      [integer(Id)],
                      none,
                      [tuple([atom(ok), application(IdAst, [])])])
                    |Acc]
           end,
           [clause([underscore()], none, [atom(undefined)])],
           ExportedNodes))
      ], ExportedNodes).
    
compile_node({IdAst, NameAst, Node}) ->
    Id = schema(get, id, Node),
    Fields = compile_node_type(schema(get, Node)),
    function(
      IdAst,
      [clause(
         none,
         [record_expr(
            atom(schema_node),
            [record_field(atom(name), NameAst),
             record_field(atom(id), integer(Id)),
             record_field(atom(src),
                          binary(
                            [binary_field(
                               string(
                                 binary_to_list(
                                   schema(get, displayName, Node))))
                            ]))
             |Fields])
         ])
      ]).


compile_node_type(file) ->
    [record_field(atom(kind), atom(file))];
compile_node_type({struct, Struct}) ->
    {Fields, Union} = lists:partition(
                        fun (F) ->
                                16#ffff == schema(get, discriminantValue, F)
                        end,
                        schema(get, fields, Struct)),
    Sizes = [record_field(
               atom(RecordKey),
               F(schema(get, CapnpKey, Struct)))
             || {RecordKey, CapnpKey, F} <- 
                    [{esize, preferredListEncoding, fun erl_syntax:atom/1},
                     {psize, pointerCount, fun erl_syntax:integer/1},
                     {dsize, dataWordCount, fun erl_syntax:integer/1}]],
    [record_field(
       atom(kind),
       record_expr(
         atom(struct),
         lists:reverse(
           [record_field(
              atom(fields),
              list([compile_struct_field(M)
                    || M <- Fields])),
            record_field(
              atom(union_field),
              compile_union(Union, Struct))
            |Sizes
           ])))
    ];
compile_node_type({enum, Enum}) ->
    Enumerants = [binary_to_list(schema(get, name, E))
                  || E <- schema(get, enumerants, Enum)],
    Values = [tuple([integer(Key), atom(Value)])
              || {Key, Value} <-
                     lists:zip(
                       lists:seq(0, length(Enumerants) - 1),
                       Enumerants)],
    [record_field(
       atom(kind),
       record_expr(
         atom(enum),
         [record_field(atom(values), list(Values))]
        ))
    ];
compile_node_type({interface, Interface}) ->
    [record_field(
       atom(kind),
       record_expr(
         atom(interface),
         [record_field(
            atom(extends),
            list([integer(Id) || Id <- schema(get, extends, Interface)])),
          record_field(
            atom(methods),
            list([record_expr(
                    atom(method),
                    [record_field(atom(name), atom(binary_to_list(schema(get, name, M)))),
                     record_field(atom(paramType), integer(schema(get, paramStructType, M))),
                     record_field(atom(resultType), integer(schema(get, resultStructType, M)))
                    ])
                  || M <- schema(get, methods, Interface)]))
          %% there is a `#interface.struct` field too.. but not sure if it's a keeper.. (see ecapnpc.erl)
         ]))
    ];
compile_node_type({const, Const}) ->
    Type = schema(get, type, Const),
    Value = schema(get, value, Const),
    [record_field(
       atom(kind),
       record_expr(
         atom(const),
         [record_field(
            atom(field),
            compile_slot_field(Type, 0, Value))
         ]))
    ];
compile_node_type({annotation, Annotation}) ->
    Type = schema(get, type, Annotation),
    [record_field(
       atom(kind),
       record_expr(
         atom(annotation),
         [record_field(
            atom(type),
            compile_slot_field(Type, 0, null)),
          record_field(
            atom(targets),
            list([atom(T) || T <- [targetsFile, targetsConst, targetsEnum,
                                   targetsEnumerant, targetsStruct, targetsField,
                                   targetsUnion, targetsGroup, targetsInterface,
                                   targetsMethod, targetsParam, targetsAnnotation
                                  ],
                             schema(get, T, Annotation)
                 ]))
         ]))
    ];
compile_node_type({NodeKind, _}) ->
    throw({unknown_node_kind, NodeKind}).

compile_union(Union, Struct) ->
    case schema(get, discriminantCount, Struct) of
        0 -> atom(none);
        _ ->
            UnionFields = lists:zip(
                            lists:seq(0, length(Union) - 1),
                            Union),
            compile_data_field(
              {{union,
                list([begin
                          T = compile_struct_field(F),
                          case erl_syntax_lib:analyze_record_expr(T) of
                              {record_expr, {field, [{name, Name}|_]}} ->
                                  tuple([integer(I), Name, T])
                          end
                      end || {I, F} <- UnionFields])}, 16},
              schema(get, discriminantOffset, Struct),
              {union, 0})
    end.

compile_struct_field(Field) ->
    record_expr(
      atom(field),
      [record_field(atom(name), atom(binary_to_list(schema(get, name, Field))))
       |compile_struct_field_type(schema(get, Field))]
     ).

compile_struct_field_type({slot, Slot}) ->
    [record_field(atom(kind), compile_slot(Slot))];
compile_struct_field_type({group, Group}) ->
    [record_field(
       atom(kind),
       record_expr(
         atom(group),
         [record_field(
            atom(id),
            integer(schema(get, typeId, Group)))
         ]))
    ].

compile_slot(Slot) ->
    [Type, Offset, Default]
        = [schema(get, Key, Slot)
           || Key <- [type, offset, defaultValue]],
    compile_slot_field(Type, Offset, Default).

compile_slot_field(Type, Offset, Default) ->
    case capnp_type_info(schema(get, Type)) of
        {data_field, DataType} -> compile_data_field(DataType, Offset, Default);
        {ptr_field, PtrType} -> compile_ptr_field(PtrType, Offset, Default)
    end.

compile_data_field(void, _Offset, _Default) -> atom(void);
compile_data_field({Type, 1}, Offset, Default) ->
    %% compensate for erlangs big endian bit streams.. yuck!
    compile_data_field({Type, 8}, (Offset div 8) + ((7 - (Offset rem 8))/8), Default);
compile_data_field({Type, Size}, Offset, Default) ->
    record_expr(
      atom(data),
      [record_field(atom(type), compile_field_type(Type)),
       record_field(atom(align), integer(round(Size * Offset))),
       record_field(atom(default), compile_value(Type, Default))
      ]).

compile_ptr_field(Type, Index, Default) ->
    record_expr(
      atom(ptr),
      [record_field(atom(type), compile_field_type(Type)),
       record_field(atom(idx), integer(Index)),
       record_field(atom(default), compile_value(Type, Default))
      ]).

compile_field_type({list, Type}) ->
    tuple([atom(list), compile_field_type(Type)]);
compile_field_type({struct, Type}) ->
    tuple([atom(struct), integer(Type)]);
compile_field_type({union, Fields}) ->
    tuple([atom(union), Fields]);
compile_field_type({enum, Type}) ->
    tuple([atom(enum), integer(Type)]);
compile_field_type(Type) -> atom(Type).

compile_value(Type, null) ->
    if Type == text; Type == data -> binary([]);
       true ->
            case Type of
                {Simple, _} when Simple == union; Simple == enum ->
                    binary([binary_field(integer(0), integer(16), [atom(integer)])]);
                _ ->
                    binary([binary_field(integer(0), integer(64), [atom(integer)])])
            end
    end;
compile_value(Type, #object{ schema=#schema_node{ name='Value' } }=Object) ->
    compile_value(Type, schema(get, Object));
compile_value(_Type, {_, Object}) when is_record(Object, object) ->
    compile_binary(ecapnp_obj:copy(Object));
compile_value({Type, _}, {Type, Value})
  when Type == union; Type == enum ->
    compile_binary(ecapnp_val:set(uint16, Value));
compile_value(Type, {Type, Value})
  when Type == text; Type == data ->
    compile_binary(Value);
compile_value(Type, {ValueType, Value})
  when Type == ValueType; element(1, Type) == ValueType ->
    compile_binary(ecapnp_val:set(ValueType, Value));
compile_value({interface, _}, interface) ->
    binary([binary_field(integer(0), integer(64), atom(integer))]);
compile_value(Type, Value) ->
    throw({value_type_mismatch, Type, Value}).

compile_binary(Bin) ->
    Fields = lists:foldl(
               fun (B, Acc) when is_integer(B) -> [binary_field(integer(B))|Acc];
                   (B, Acc) when is_bitstring(B) ->
                       S = bit_size(B),
                       <<V:S>> = B,
                       [binary_field(integer(V), integer(S), [])|Acc]
               end, [], bitstring_to_list(Bin)),
    binary(Fields).


capnp_type_info(void) -> {data_field, void};
capnp_type_info(bool) -> {data_field, {bool, 1}};
capnp_type_info(int8) -> {data_field, {int8, 8}};
capnp_type_info(int16) -> {data_field, {int16, 16}};
capnp_type_info(int32) -> {data_field, {int32, 32}};
capnp_type_info(int64) -> {data_field, {int64, 64}};
capnp_type_info(uint8) -> {data_field, {uint8, 8}};
capnp_type_info(uint16) -> {data_field, {uint16, 16}};
capnp_type_info(uint32) -> {data_field, {uint32, 32}};
capnp_type_info(uint64) -> {data_field, {uint64, 64}};
capnp_type_info(float32) -> {data_field, {float32, 32}};
capnp_type_info(float64) -> {data_field, {float64, 64}};
capnp_type_info(text) -> {ptr_field, text};
capnp_type_info(data) -> {ptr_field, data};
capnp_type_info(object) -> {ptr_field, object};
capnp_type_info(anyPointer) -> {ptr_field, object};
capnp_type_info({list, List}) -> {ptr_field, {list, list_field_type(List)}};
capnp_type_info({enum, Enum}) -> {data_field, {{enum, schema(get, typeId, Enum)}, 16}};
capnp_type_info({RefT, Obj})
  when RefT == struct; RefT == interface ->
    {ptr_field, {RefT, schema(get, typeId, Obj)}};
capnp_type_info(Other) -> throw({unknown_capnp_type, Other}).

list_field_type(List) ->
    Type = schema(get, elementType, List),
    case capnp_type_info(schema(get, Type)) of
        {data_field, {T, _}} -> T;
        {_, T} -> T
    end.

get_node(Id, Nodes) ->
    [Node] = [N || N <- Nodes, Id =:= schema(get, id, N)],
    Node.

get_exported_nodes(Id, Nodes) ->
    get_nested_nodes(Id, Nodes, [], []).

get_nested_nodes(Id, Nodes, Scope, Acc) ->
    Node = get_node(Id, Nodes),
    NestedNodes = schema(get, nestedNodes, Node),
    lists:foldr(
      fun (N, NestedAcc) ->
              NId = schema(get, id, N),
              Name = schema(get, name, N),
              get_exported_node({NId, Name}, Nodes, Scope, NestedAcc)
      end, Acc, NestedNodes).

get_exported_node({Id, Name}, Nodes, Scope, Acc) ->
    IdAst = atom(integer_to_list(Id)),
    NameAst = atom(lists:flatten(io_lib:format("~s", [[Scope, Name]]))),
    Schema = get_node(Id, Nodes),
    Scope1 = [Scope, Name, "."],
    Acc1 = case schema(get, Schema) of
               {struct, S} ->
                   lists:foldl(
                     fun (F, AccG) ->
                             case schema(get, F) of
                                 {group, G} ->
                                     GId = schema(get, typeId, G),
                                     get_exported_node(
                                       {GId, schema(get, name, F)},
                                       Nodes,
                                       Scope1,
                                       get_nested_nodes(GId, Nodes, Scope1, AccG));
                                 _ -> AccG
                             end
                     end, Acc, schema(get, fields, S));
               _ -> Acc
           end,
    [{IdAst, NameAst, Schema}
     |get_nested_nodes(Id, Nodes, Scope1, Acc1)].

