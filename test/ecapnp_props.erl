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

value() -> union(
             [?LET(B, boolean(), {bool, B}),
              ?LET(I, integer(-128, 127), {int8, I})
             ]).

%% generate struct def with valid offset and size
struct_data() -> ?SIZED(D, struct_data(D)).
struct_data(0) -> struct_data(1); %% we need at least 1 word of data to work with
struct_data(D) -> ?SIZED(P, struct_data(D, P)).
struct_data(D, P) -> ?LET(O, range(0, (D*64) - 2),
                         ?LET(S, range(1, (D*64) - O),
                             ?LET(V, bitstring(S),
                                 {D, P, O, S, V}))).

struct_ptr() -> ?SIZED(D, struct_ptr(D)).
struct_ptr(D) -> ?SIZED(P, struct_ptr(D, P)).
struct_ptr(D, 0) -> struct_ptr(D, 1); %% we need at least 1 ptr to work with
struct_ptr(D, P) -> ?LET(I, range(0, P - 1),
                         ?LET(K, ref_kind(), {D, P, I, K})).

ref_kind() -> union(
                [struct_ref()
                 %% TODO: list_ref(), far_ref()
                ]).

struct_ref() -> ?SUCHTHAT(R, struct_ref1(),
                          begin
                              #struct_ref{ dsize=D, psize=P } = R,
                              D + P > 0
                          end).

struct_ref1() -> ?SIZED(D, struct_ref1(D)).
struct_ref1(D) -> ?SIZED(P, struct_ref1(D, P)).
struct_ref1(D, P) -> #struct_ref{ dsize=D, psize=P }.

%% this is awfully similiar to the struct_ptr() type..
text_data() -> ?SIZED(D, text_data(D)).
text_data(D) -> ?SIZED(P, text_data(D, P)).
text_data(D, 0) -> text_data(D, 1);
text_data(D, P) -> ?LET(I, range(0, P - 1),
                       ?LET(T, binary(), {D, P, I, T})).


%%% ----------------------------------------
prop_capnp_value() ->
    ?FORALL(
       {T, V}, value(),
       V =:= ecapnp_val:get(T, ecapnp_val:set(T, V))).

%%% ----------------------------------------
prop_struct_data() ->
    ?FORALL(
       {Data, Ptr, Off, Size, Value}, struct_data(),
       begin
           D = data(Data, Ptr),
           Ref = ecapnp_ref:get(0, 0, D),
           ok = ecapnp_ref:write_struct_data(Off, Size, Value, Ref),
           Value =:= ecapnp_ref:read_struct_data(Off, Size, Ref)
       end).

%%% ----------------------------------------
prop_struct_ptr() ->
    ?FORALL(
       {Data, Ptr, Idx, Kind}, struct_ptr(),
       begin
           D = data(Data, Ptr),
           Root = ecapnp_ref:get(0, 0, D),
           #ref{ kind=null }=Ref = ecapnp_ref:read_struct_ptr(Idx, Root),
           ok = ecapnp_ref:write_struct_ptr(Ref#ref{ kind=Kind }, Root),
           #ref{ kind=Kind } = ecapnp_ref:read_struct_ptr(Idx, Root),
           true
       end).


%%% ----------------------------------------
prop_text_data() ->
    ?FORALL(
       {Data, Ptr, Idx, Text}, text_data(),
       begin
           S = size(Text) + 8,
           Root = root(data(Data, Ptr, <<0:S/integer-unit:8>>)),
           %% TODO: if write_text would take a ptr index, we wouldn't
           %% need to get the ref for the text first!
           Ref0 = ecapnp_ref:read_struct_ptr(Idx, Root),
           ok = ?T(ecapnp_ref:write_text(?T(Text), ?T(Ref0), ?T(Root))),
           %% TODO: if read_text would take a ptr index, we wouldn't
           %% need to get the ref for the text first!
           Ref1 = ecapnp_ref:read_struct_ptr(Idx, Root),
           Text =:= ?T(ecapnp_ref:read_text(?T(Ref1)))
       end).

%%% ----------------------------------------
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
