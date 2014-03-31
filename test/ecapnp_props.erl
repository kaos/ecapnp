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
-include_lib("ecapnp/include/ecapnp.hrl").

-define(TRACE(E),
        fun (__V) ->
                io:format("TRACE: ~s:~b ~s = ~p~n", [?FILE, ?LINE, ??E, __V]),
                __V
        end(E)).

-define(T(E), ?TRACE(E)).


%%% ----------------------------------------
%% property types
%%% ----------------------------------------

value() -> union(
             [?LET(B, boolean(), {bool, B}),
              ?LET(I, integer(-128, 127), {int8, I})
             ]).

%% struct ref with at least one word of data
struct_ref_data() -> struct_ref(range(1, inf), range(0, inf)).
%% struct ref with at least one pointer
struct_ref_ptr() -> struct_ref(range(0, inf), range(1, inf)).

%% any struct ref, even null
struct_ref() -> struct_ref(range(0, inf), range(0, inf)).

%% generate struct ref in a given size range for data and pointer sections
struct_ref(Td, Tp) -> ?LET([D, P], [Td, Tp], #struct_ref{ dsize=D, psize=P }).

struct_data_value(R) ->
    ?LET(O, range(0, (R#struct_ref.dsize * 64) - 2),
         ?LET(S, range(1, (R#struct_ref.dsize * 64) - O),
              ?LET(V, bitstring(S),
                   {O, S, V}))).

%% generate offset and size for struct ref
struct_data() -> ?LET(R, struct_ref_data(),
                      {R, struct_data_value(R)}).

struct_ptr_idx(R) -> ?LET(I, range(0, R#struct_ref.psize - 1), I).

%% generate ptr ref and idx for struct ref
struct_ptr() -> ?LET([R, K], [struct_ref_ptr(), ref_kind()],
                     {R, struct_ptr_idx(R), K}).

%% generate a ref
ref_kind() -> union(
                [struct_ref()
                 %% TODO: list_ref(), far_ref()
                ]).

%% generate text and idx for struct ref
text_data() -> ?LET([R, T], [struct_ref_ptr(), binary()],
                    {R, struct_ptr_idx(R), T}).


%%% ----------------------------------------
%% property tests
%%% ----------------------------------------

%%% ----------------------------------------
prop_capnp_value() ->
    ?FORALL(
       {T, V}, value(),
       V =:= ecapnp_val:get(T, ecapnp_val:set(T, V))).

%%% ----------------------------------------
prop_struct_data() ->
    ?FORALL(
       {#struct_ref{ dsize=Data, psize=Ptr },
        {Off, Size, Value}}, struct_data(),
       begin
           D = data(Data, Ptr),
           Ref = ecapnp_ref:get(0, 0, D),
           ok = ecapnp_ref:write_struct_data(Off, Size, Value, Ref),
           Value =:= ecapnp_ref:read_struct_data(Off, Size, Ref)
       end).

%%% ----------------------------------------
prop_struct_ptr() ->
    ?FORALL(
       {#struct_ref{ dsize=Data, psize=Ptr },
        Idx, Kind}, struct_ptr(),
       begin
           Root = root(data(Data, Ptr)),
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
       {#struct_ref{ dsize=Data, psize=Ptr},
        Idx, Text}, text_data(),
       begin
           S = size(Text) + 8,
           Root = root(data(Data, Ptr, <<0:S/integer-unit:8>>)),
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


%%% ----------------------------------------
%% helpers
%%% ----------------------------------------

%% create new message with provided data segments
data(Data) ->
    data(Data, [size(S) || S <- Data]).

data(Data, Alloc) when length(Data) =:= length(Alloc) ->
    ecapnp_data:new(#msg{ data=Data, alloc=Alloc });

%% create new message with empty root of given data and pointer section sizes
data(Data, Ptr) when is_integer(Data), is_integer(Ptr) -> data(Data, Ptr, <<>>).

data(Data, Ptr, Extra) ->
    data([<<0:32, Data:16/integer-little, Ptr:16/integer-little,
            0:Data/integer-unit:64, 0:Ptr/integer-unit:64,
            Extra/binary>>],
         [8 * (Data + Ptr + 1)]).

%% create new message and get a ref to the root element
root(Data) when is_pid(Data) ->
    ecapnp_ref:get(0, 0, Data).

root(Data, Ptr) ->
    root(data(Data, Ptr)).
