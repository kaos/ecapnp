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

-import(erl_syntax, [atom/1, atom_name/1, function/2, clause/2,
                     record_expr/2, clause/3, record_field/2,
                     binary/1, underscore/0, binary_field/1,
                     application/2, application/3, form_list/1,
                     set_precomments/2, set_postcomments/2, comment/1,
                     string/1, integer/1, tuple/1, arity_qualifier/2,
                     binary_field/3, list/1, attribute/2, macro/1,
                     variable/1]).

-include("ecapnp.hrl").

-type compiled_message() :: {file, erl_syntax:syntaxTree()}.

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Compile a `CodeGeneratorRequest' message.
-spec compile( message() ) -> {ok, [compiled_message()]}.
compile(Message)
  when is_list(Message), is_binary(hd(Message)) ->
    {ok, Root} = ecapnp:get_root('CodeGeneratorRequest', schema_capnp, Message),
    compile_root(Root).


%% ===================================================================
%% internal functions
%% ===================================================================

compile_root(Root) ->
    Nodes = ecapnp:get(nodes, Root),
    Files = ecapnp:get(requestedFiles, Root),
    {ok, [compile_file(File, Nodes) || File <- Files]}.

compile_file(File, Nodes) ->
    Id = ecapnp:get(id, File),
    FileNodes = get_file_nodes(Id, Nodes),
    Filename = compile_filename(ecapnp:get(filename, File)),
    Imports = compile_imports(File, Nodes),

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
       lists:flatten(
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
          attribute(atom(export), [compile_exports(FileNodes)]),
          attribute(atom(types), [compile_types(FileNodes)]),
          compile_import_attributes(Imports),
          attribute(atom(include_lib), [string("ecapnp/include/ecapnp.hrl")]),
          compile_schema_fun(FileNodes ++ Imports)
          |compile_nodes(FileNodes)
         ]))}.

compile_filename(Filename) ->
    B = binary:replace(
          filename:rootname(Filename, <<".capnp">>),
          <<".">>, <<"_">>, [global]),
    <<B/binary, "_capnp">>.

compile_modulename(Filename) ->
    atom(binary_to_list(filename:basename(Filename))).

compile_exports(FileNodes) ->
    Exports = lists:foldr(
                fun ({IdAst, [NameAst], _Schema}, Acc) ->
                        [arity_qualifier(NameAst, integer(0)),
                         arity_qualifier(NameAst, integer(1)),
                         arity_qualifier(IdAst, integer(0))|Acc];
                    ({IdAst, _, _Schema}, Acc) ->
                        [arity_qualifier(IdAst, integer(0))|Acc]
                end, [], FileNodes),
    list([arity_qualifier(atom(schema), integer(1))|Exports]).

compile_types(FileNodes) ->
    Types = [begin
                 Id = ecapnp:get(id, Schema),
                 NameAst = case Scope of
                               [N] -> N;
                               _ -> list(lists:reverse(Scope))
                           end,
                 tuple([integer(Id), NameAst])
             end || {_IdAst, Scope, Schema} <- FileNodes],
    list(Types).

compile_schema_fun(FileNodes) ->
    function(
      atom(schema),
      compile_schema_fun_clauses(
        FileNodes,
        [clause([underscore()], none, [atom(undefined)])])).

compile_schema_fun_clauses(Nodes, Acc0) ->
    lists:foldr(
      fun ({IdAst, Scope, Schema}, Acc) ->
              Id = ecapnp:get(id, Schema),
              case Scope of
                  [NameAst] ->
                      [clause([integer(Id)], none, [application(IdAst, [])]),
                       clause([NameAst], none, [application(IdAst, [])]),
                       clause([list([NameAst])], none, [application(IdAst, [])])
                       |Acc];
                  _ ->
                      [clause([integer(Id)], none, [application(IdAst, [])]),
                       clause([list(lists:reverse(Scope))], none, [application(IdAst, [])])
                       |Acc]
              end;
          ({ImportModAst, ImportNodes}, Acc) ->
              case compile_schema_fun_clauses(ImportNodes, Acc) of
                  Acc -> Acc;
                  [N|Ns] ->
                      [set_precomments(
                         N, [comment([io_lib:format(
                                        "% Imported from ~s",
                                        [atom_name(ImportModAst)])])])
                       |Ns]
              end
      end, Acc0, Nodes).

compile_import_attributes(Imports) ->
    lists:foldr(
      fun ({ModAst, Nodes}, AccImports) ->
              attribute(
                atom(import),
                [ModAst,
                 list(
                   lists:foldr(
                     fun ({IdAst, _Scope, _Schema}, Acc) ->
                             [arity_qualifier(IdAst, integer(0))|Acc]
                     end, AccImports, Nodes))])
      end, [], Imports).

compile_imports(File, Nodes) ->
    lists:foldl(
      fun (Import, Acc) ->
              Mod = compile_modulename(
                      compile_filename(
                        ecapnp:get(name, Import))),
              [{Mod, get_nested_nodes(ecapnp:get(id, Import), Nodes, [], [])}
               |Acc]
      end, [], ecapnp:get(imports, File)).

compile_nodes(FileNodes) ->
    compile_nodes(FileNodes, [], []).

compile_nodes([{IdAst, [NameAst], Schema}|FileNodes], [], Acc) ->
    {FileNodes1, {Cl, Fs}} = compile_nodes(
                               FileNodes, [NameAst],
                               {[clause([list([])], none, [application(IdAst, [])])],
                                Acc}),
    Fun = function(NameAst, [clause(none, [application(IdAst, [])])]),
    Node = compile_node({IdAst, NameAst, Schema}),
    Acc1 = [Fun, function(NameAst, Cl), Node|Fs],
    compile_nodes(FileNodes1, [], Acc1);
compile_nodes([{IdAst, [_|Scope]=RPath, Schema}|FileNodes], Scope, Acc) ->
    {FileNodes1, {Cl, Fs}} = compile_nodes(FileNodes, RPath, Acc),
    Path = lists:reverse(RPath),
    F = compile_node({IdAst, list(Path), Schema}),
    compile_nodes(FileNodes1, Scope,
                  {[clause([list(tl(Path))], none,
                           [application(IdAst, [])])
                    |Cl], [F|Fs]});
compile_nodes([{_, [_], _}|_]=FileNodes, [_|_], Acc) ->
    {FileNodes, Acc};
compile_nodes([], [_|_], Acc) ->
    {[], Acc};
compile_nodes([], [], Acc) ->
    Acc;
compile_nodes(FileNodes, [], Acc) ->
    {FileNodes, Acc};
compile_nodes(FileNodes, [_|Scope], Acc) ->
    compile_nodes(FileNodes, Scope, Acc).

compile_node({IdAst, NameAst, Node}) ->
    function(
      IdAst,
      [clause(
         none,
         [record_expr(
            atom(schema_node),
            lists:foldr(
              fun (Fun, Acc) -> Fun(Acc) end, [],
              [fun (Acc) ->
                       Id = ecapnp:get(id, Node),
                       ScopeId = ecapnp:get(scopeId, Node),
                       [record_field(atom(module), macro(variable('MODULE'))),
                        record_field(atom(name), NameAst),
                        record_field(atom(id), integer(Id)),
                        record_field(atom(scope), integer(ScopeId)),
                        record_field(atom(src),
                                     binary(
                                       [binary_field(
                                          string(
                                            binary_to_list(
                                              ecapnp:get(displayName, Node))))
                                       ]))|Acc]
               end,
               fun (Acc) ->
                       compile_annotation_field(
                         ecapnp:get(annotations, Node),
                         annotations, Acc)
               end,
               fun (Acc) ->
                       [compile_node_type(ecapnp:get(Node))|Acc]
               end,
               fun (Acc) ->
                       case ecapnp:get(nestedNodes, Node) of
                           [] -> Acc;
                           Nested ->
                               [record_field(
                                  atom(nodes),
                                  list([begin
                                            Id = ecapnp:get(id, N),
                                            Name = ecapnp:get(name, N),
                                            set_postcomments(
                                              integer(Id),
                                              [comment([io_lib:format("% ~s", [Name])])])
                                        end || N <- Nested]))
                                |Acc]
                       end
               end]))
         ])
      ]).

compile_annotation_field([], _FieldName, Acc) -> Acc;
compile_annotation_field(Annotations, FieldName, Acc) ->
    [record_field(
       atom(FieldName),
       list(compile_annotations(Annotations)))
    |Acc].

compile_annotations(Annotations) ->
    [compile_annotation(A) || A <- Annotations].

compile_annotation(Annotation) ->
    Id = ecapnp:get(id, Annotation),
    Value = ecapnp:get(value, Annotation),
    tuple([integer(Id), compile_value(any, ecapnp:get(Value))]).

compile_node_type(file) ->
    record_field(atom(kind), atom(file));
compile_node_type({struct, Struct}) ->
    {Fields, Union} = lists:partition(
                        fun (F) ->
                                16#ffff == ecapnp:get(discriminantValue, F)
                        end,
                        ecapnp:get(fields, Struct)),
    Sizes = [record_field(
               atom(RecordKey),
               F(ecapnp:get(CapnpKey, Struct)))
             || {RecordKey, CapnpKey, F} <-
                    [{esize, preferredListEncoding, fun erl_syntax:atom/1},
                     {psize, pointerCount, fun erl_syntax:integer/1},
                     {dsize, dataWordCount, fun erl_syntax:integer/1}]],
    record_field(
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
          ])));
compile_node_type({enum, Enum}) ->
    Enumerants = [binary_to_list(ecapnp:get(name, E))
                  || E <- ecapnp:get(enumerants, Enum)],
    Values = [tuple([integer(Key), atom(Value)])
              || {Key, Value} <-
                     lists:zip(
                       lists:seq(0, length(Enumerants) - 1),
                       Enumerants)],
    record_field(
      atom(kind),
      record_expr(
        atom(enum),
        [record_field(atom(values), list(Values))]
       ));
compile_node_type({interface, Interface}) ->
    record_field(
      atom(kind),
      record_expr(
        atom(interface),
        [record_field(
           atom(extends),
           list([integer(Id) || Id <- ecapnp:get(extends, Interface)])),
         record_field(
           atom(methods),
           list([record_expr(
                   atom(method),
                   [record_field(atom(name), atom(binary_to_list(ecapnp:get(name, M)))),
                    record_field(atom(paramType), integer(ecapnp:get(paramStructType, M))),
                    record_field(atom(resultType), integer(ecapnp:get(resultStructType, M)))
                   ])
                 || M <- ecapnp:get(methods, Interface)]))
         %% there is a `#interface.struct` field too.. but not sure if it's a keeper.. (see ecapnpc.erl)
        ]));
compile_node_type({const, Const}) ->
    Type = ecapnp:get(type, Const),
    Value = ecapnp:get(value, Const),
    record_field(
      atom(kind),
      record_expr(
        atom(const),
        [record_field(
           atom(field),
           compile_slot_field(Type, 0, Value))
        ]));
compile_node_type({annotation, Annotation}) ->
    Type = ecapnp:get(type, Annotation),
    record_field(
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
                            ecapnp:get(T, Annotation)
                ]))
        ]));
compile_node_type({NodeKind, _}) ->
    throw({unknown_node_kind, NodeKind}).

compile_union(Union, Struct) ->
    case ecapnp:get(discriminantCount, Struct) of
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
              ecapnp:get(discriminantOffset, Struct),
              {union, 0})
    end.

compile_struct_field(Field) ->
    record_expr(
      atom(field),
      [record_field(atom(name), atom(binary_to_list(ecapnp:get(name, Field))))
       |compile_struct_field_type(ecapnp:get(Field))]
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
            integer(ecapnp:get(typeId, Group)))
         ]))
    ].

compile_slot(Slot) ->
    [Type, Offset, Default]
        = [ecapnp:get(Key, Slot)
           || Key <- [type, offset, defaultValue]],
    compile_slot_field(Type, Offset, Default).

compile_slot_field(Type, Offset, Default) ->
    case capnp_type_info(ecapnp:get(Type)) of
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

compile_value(any, {Type, _}=Value) ->
    compile_value(Type, Value);
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
    compile_value(Type, ecapnp:get(Object));
compile_value(_Type, {_, Object}) when is_record(Object, object) ->
    compile_binary(ecapnp_obj:copy(Object));
compile_value({Type, _}, {Type, Value})
  when Type == union; Type == enum ->
    compile_binary(ecapnp_val:set(uint16, Value));
compile_value(Type, {Type, Value})
  when Type == text; Type == data ->
    compile_binary(Type, Value);
compile_value(Type, {ValueType, Value})
  when Type == ValueType; element(1, Type) == ValueType ->
    compile_binary(ecapnp_val:set(ValueType, Value));
compile_value({interface, _}, interface) ->
    binary([binary_field(integer(0), integer(64), atom(integer))]);
compile_value(Type, Value) ->
    throw({value_type_mismatch, Type, Value}).


compile_binary(text, Bin) ->
    binary([binary_field(string(binary_to_list(Bin)))]);
compile_binary(data, Bin) ->
    compile_binary(Bin).

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
capnp_type_info({enum, Enum}) -> {data_field, {{enum, ecapnp:get(typeId, Enum)}, 16}};
capnp_type_info({RefT, Obj})
  when RefT == struct; RefT == interface ->
    {ptr_field, {RefT, ecapnp:get(typeId, Obj)}};
capnp_type_info(Other) -> throw({unknown_capnp_type, Other}).

list_field_type(List) ->
    Type = ecapnp:get(elementType, List),
    case capnp_type_info(ecapnp:get(Type)) of
        {data_field, {T, _}} -> T;
        {_, T} -> T
    end.

get_node(Id, Nodes) ->
    [Node] = [N || N <- Nodes, Id =:= ecapnp:get(id, N)],
    Node.

get_file_nodes(Id, Nodes) ->
    IdAst = atom(integer_to_list(Id)),
    NameAst = atom(root),
    Schema = get_node(Id, Nodes),
    get_nested_nodes(Id, Nodes, [], [{IdAst, [NameAst], Schema}]).

get_nested_nodes(Id, Nodes, Scope, Acc) ->
    Node = get_node(Id, Nodes),
    NestedNodes = ecapnp:get(nestedNodes, Node),
    lists:foldr(
      fun (N, NestedAcc) ->
              NId = ecapnp:get(id, N),
              Name = ecapnp:get(name, N),
              get_exported_node({NId, Name}, Nodes, Scope, NestedAcc)
      end, Acc, NestedNodes).

get_exported_node({Id, Name}, Nodes, Scope, Acc) ->
    NameAst = atom(binary_to_list(Name)),
    Scope1 = [NameAst|Scope],
    Schema = get_node(Id, Nodes),
    Acc1 = case ecapnp:get(Schema) of
               {struct, S} ->
                   lists:foldl(
                     fun (F, AccG) ->
                             case ecapnp:get(F) of
                                 {group, G} ->
                                     GId = ecapnp:get(typeId, G),
                                     get_exported_node(
                                       {GId, ecapnp:get(name, F)},
                                       Nodes,
                                       Scope1,
                                       get_nested_nodes(GId, Nodes, Scope1, AccG));
                                 _ -> AccG
                             end
                     end, Acc, ecapnp:get(fields, S));
               _ -> Acc
           end,
    IdAst = atom(integer_to_list(Id)),
    [{IdAst, Scope1, Schema}
     |get_nested_nodes(Id, Nodes, Scope1, Acc1)].
