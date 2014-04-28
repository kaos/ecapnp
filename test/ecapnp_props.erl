%%
%%  Copyright 2014, Andreas Stenius <kaos@astekk.se>
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

-module(ecapnp_props).

-include_lib("proper/include/proper.hrl").
-include("include/ecapnp.hrl").

-define(TRACE(E),
        fun (__V) ->
                io:format("TRACE: ~s:~b ~s = ~p~n", [?FILE, ?LINE, ??E, __V]),
                __V
        end(E)).

-define(T(E), ?TRACE(E)).


%%% ----------------------------------------
%% property types
%%% ----------------------------------------

%% generate any kind of primitive value that capnp can represent.
data_value() ->
    union(
      [?LET(B, boolean(), {1, bool, B}),
       ?LET(I, integer(-16#80, 16#7f), {8, int8, I}),
       ?LET(I, integer(-16#8000, 16#7fff), {16, int16, I}),
       ?LET(I, integer(-16#80000000, 16#7fffffff), {32, int32, I}),
       ?LET(I, integer(-16#8000000000000000, 16#7fffffffffffffff), {64, int64, I}),
       ?LET(I, integer(0, 16#ff), {8, uint8, I}),
       ?LET(I, integer(0, 16#ffff), {16, uint16, I}),
       ?LET(I, integer(0, 16#ffffffff), {32, uint32, I}),
       ?LET(I, integer(0, 16#ffffffffffffffff), {64, uint64, I}),
       ?LET(F, float(-3.4e38, 3.4e38), {32, float32, F}), %% bound it to just under 32 bit
       ?LET(F, float(), {64, float64, F}), %% erlang floats are 64 bit
       ?LET([{S, T}, V], %% test infinity and NaN values..
            [union([{32, float32}, {64, float64}]),
             union([nan, inf, '-inf'])],
            {S, T, V})
      ]).

ptr_value() ->
    union(
      [?LET({T, V}, list_ptr(), {{list, T}, V, <<0,0,0,0,0,0,0,0>>}),
       ?LET(V, binary(), {text, V, <<>>})
      ]).

list_ptr() ->
    union(
      [{bool, list(boolean())},
       {int8, list(integer(-16#80, 16#7f))},
       {int16, list(integer(-16#8000, 16#7fff))},
       {int32, list(integer(-16#80000000, 16#7fffffff))},
       {int64, list(integer(-16#8000000000000000, 16#7fffffffffffffff))},
       {uint8, list(integer(0, 16#ff))},
       {uint16, list(integer(0, 16#ffff))},
       {uint32, list(integer(0, 16#ffffffff))},
       {uint64, list(integer(0, 16#ffffffffffffffff))},
       %% avoid floats, as they don't compare well..
       %%{float32, list(float(-3.4e38, 3.4e38))},
       %%{float64, list(float())},
       {text, list(binary())}
      ]).

%% struct ref with at least one word of data
struct_ref_data() -> struct_ref(pos_integer(), non_neg_integer()).
%% struct ref with at least one pointer
struct_ref_ptr() -> struct_ref(non_neg_integer(), pos_integer()).

%% any struct ref, even null
struct_ref() -> struct_ref(range(0, inf), range(0, inf)).

%% generate struct ref in a given size range for data and pointer sections
struct_ref(Td, Tp) -> ?LET([D, P], [Td, Tp], #struct_ref{ dsize=D, psize=P }).

%% %% generate list ref
%% list_ref() -> ?LET([S, C], [union([empty]), non_neg_integer()],
%%                    #list_ref{ size=S, count=C }).

%% generate random field data that will fit in given struct ref
struct_data_value(R) ->
    ?LET(O, range(0, (R#struct_ref.dsize * 64) - 2),
         ?LET(S, range(1, (R#struct_ref.dsize * 64) - O),
              ?LET(V, bitstring(S),
                   {O, S, V}))).

%% generate struct ref and random field data at some offset
struct_data() -> ?LET(R, struct_ref_data(),
                      {R, struct_data_value(R)}).

%% generate ptr index for given struct ref
struct_ptr_idx(R) -> ?LET(I, range(0, R#struct_ref.psize - 1), I).

%% generate struct ref and ptr ref to another struct at idx
struct_ptr() -> ?LET([R, K], [struct_ref_ptr(), struct_ref()],
                     {R, struct_ptr_idx(R), K}).

%% %% generate a ptr ref
%% ref_kind() -> union(
%%                 [struct_ref(),
%%                  list_ref()
%%                  %% TODO: far_ref, interface_ref
%%                 ]).

%% generate struct ref and text at idx
text_data() -> ?LET([R, T], [struct_ref_ptr(), binary()],
                    {R, struct_ptr_idx(R), T}).

%% generate struct data field def
data_field(FieldName, Align, Type, Size) ->
    #field{
       name = FieldName,
       kind = #data{ type = Type, align = Align, default = <<0:Size>> }
      }.

%% generate list of data fields with given field names
data_fields([], Align, Fs, FVs) -> {Align, Fs, FVs};
data_fields([FieldName|FieldNames], Align, Fs, FVs) ->
    ?LET({S, T, V}, data_value(),
         data_fields(
           FieldNames, Align + S,
           [data_field(FieldName, Align, T, S)|Fs],
           [{FieldName, V}|FVs]
          )).

%% generate struct ptr field
ptr_field(FieldName, T, I, D) ->
    #field{
       name = FieldName,
       kind = #ptr{ type = T, idx = I, default = D }
      }.

%% generate list of ptr fields with given field names
ptr_fields([], Fs, FVs) -> {Fs, FVs};
ptr_fields([FieldName|FieldNames], Fs, FVs) ->
    ?LET({T, V, D}, ptr_value(),
         ptr_fields(
           FieldNames,
           [ptr_field(FieldName, T, length(Fs), D)|Fs],
           [{FieldName, V}|FVs]
          )).

%% generate schema for a struct with given data size and fields
struct_node({DSize, PSize}, Fields) ->
    ?LET(R, struct_ref(range(DSize, inf), range(PSize, inf)),
         #struct{ dsize = R#struct_ref.dsize,
                  psize = R#struct_ref.psize,
                  fields = Fields
                }).

%% generate schema node for struct with given names for data fields
schema_data_fields(FieldNames) ->
    ?LET({Size, Fs, FVs}, data_fields(FieldNames, 0, [], []),
         ?LET(S, struct_node({1 + ((Size - 1) div 8), 0}, Fs),
              {#schema_node{ kind = S }, FVs})).

schema_ptr_fields(FieldNames) ->
    ?LET({Fs, FVs}, ptr_fields(FieldNames, [], []),
         ?LET(S, struct_node({0, length(Fs)}, Fs),
              {#schema_node{ kind = S }, FVs})).

%% generate schema node for struct
schema_data_fields() ->
    ?LET(FieldNames, field_names(), schema_data_fields(FieldNames)).

%% generate field names
field_names() ->
    ?SUCHTHAT(FieldNames, non_empty(list(atom())),
              lists:sort(FieldNames) =:= lists:usort(FieldNames)).

schema_ptr_fields() ->
    ?LET(FieldNames, field_names(), schema_ptr_fields(FieldNames)).




%%% ----------------------------------------
%% property tests
%%% ----------------------------------------

%%% ----------------------------------------
prop_capnp_value() ->
    ?FORALL(
       {_S, T, V}, data_value(),
       compare_value(V, ecapnp_val:get(T, ecapnp_val:set(T, V)))).

%%% ----------------------------------------
prop_struct_data() ->
    ?FORALL(
       {Def, {Off, Size, Value}}, struct_data(),
       begin
           Root = data(Def),
           %% note: value is binary
           ok = ecapnp_ref:write_struct_data(Off, Size, Value, Root),
           Value =:= ecapnp_ref:read_struct_data(Off, Size, Root)
       end).

%%% ----------------------------------------
prop_struct_ptr() ->
    ?FORALL(
       {Def, Idx, Kind}, struct_ptr(),
       begin
           Root = data(Def),
           #ref{ kind=null }=Ref = ecapnp_ref:read_struct_ptr(Idx, Root),
           ok = ecapnp_ref:write_struct_ptr(Ref#ref{ kind=Kind }, Root),
           case ecapnp_ref:read_struct_ptr(Idx, Root) of
               #ref{ kind=Kind } -> true;
               #ref{ kind=null }
                 when Kind#struct_ref.dsize == 0,
                      Kind#struct_ref.psize == 0 ->
                   true;
               _ ->
                   false
           end
       end).

%%% ----------------------------------------
prop_text_data() ->
    ?FORALL(
       {Def, Idx, Text}, text_data(),
       begin
           Root = data(Def, 1 + ((size(Text) - 1) div 8)),
           %% TODO: if write_text would take a ptr index, we wouldn't
           %% need to get the ref for the text first!
           Ref0 = ecapnp_ref:read_struct_ptr(Idx, Root),
           ok = ecapnp_ref:write_text(Text, Ref0, Root),
           %% TODO: if read_text would take a ptr index, we wouldn't
           %% need to get the ref for the text first!
           Ref1 = ecapnp_ref:read_struct_ptr(Idx, Root),
           Text =:= ecapnp_ref:read_text(Ref1)
       end).

%%% ----------------------------------------
prop_data_field() ->
    ?FORALL(Data, schema_data_fields(), test_field_access(Data)).

prop_ptr_field() ->
    ?FORALL(Data, schema_ptr_fields(), test_field_access(Data)).

test_field_access({Schema, FVs}) ->
    %% Schema is a generated #shema_node{} record
    Root = data(Schema),
    Obj = ecapnp_obj:from_ref(Root, Schema, no_schema),
    lists:all(
      fun ({F, V}) ->
              ok = ecapnp:set(F, V, Obj),
              compare_value(V, ecapnp:get(F, Obj))
      end, FVs).

%%% ----------------------------------------
prop_find_schema() ->
    ?FORALL(
       {IdOrType, S}, schema(),
       begin
           %% find by schema module name
           ByModule = S =:= ecapnp_schema:lookup(IdOrType, schema_capnp),
           %% find by schema from same schema file
           BySchema = S =:= ecapnp_schema:lookup(IdOrType, schema_capnp:'Type'()),
           %% already got the schema..
           NoOp = S =:= ecapnp_schema:lookup(IdOrType, S),
           NoOp2 = S =:= ecapnp_schema:lookup(S, no_matter),
           ?WHENFAIL(
              io:format("Type: ~p failed lookup by: ~p~n",
                        [IdOrType,
                         [By || {By, false} <- [{module, ByModule},
                                                {schema, BySchema},
                                                {no_op, NoOp},
                                                {no_op2, NoOp2}
                                               ]]]),
              ByModule and BySchema and NoOp and NoOp2
             )
       end).

schema() ->
    union(
     [?LET({Id, _Type}, schema_types(), {Id, schema_capnp:schema(Id)}),
      ?LET({_Id, Type}, schema_types(), {Type, schema_capnp:schema(Type)})
     ]).

schema_types() ->
    union(proplists:get_value(types, schema_capnp:module_info(attributes))).


%%% ----------------------------------------
%% helpers
%%% ----------------------------------------

data(T) -> data(T, 0).

data(#struct_ref{ dsize=D, psize=P }=Kind, Extra) ->
    {ok, Pid} = ecapnp_data:start_link(1 + D + P + Extra),
    ecapnp_ref:alloc(Kind, 0, 1 + D + P, Pid);
data(#schema_node{}=N, Extra) ->
    data(ecapnp_schema:get_ref_kind(N), Extra).

compare_value(V, V) -> true;
compare_value(V, W)
  when is_float(V), is_float(W) ->
    %% allow "minor" rounding error due to representation limitations
    Err = (V-W)/V,
    (Err > -0.1e-5) and (Err < 0.1e-5);
compare_value(_V, _W) -> false.
