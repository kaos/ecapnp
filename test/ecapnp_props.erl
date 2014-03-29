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
value() -> union(
             [?LET(B, boolean(), {bool, B}),
              ?LET(I, integer(-128, 127), {int8, I})
             ]).

struct_ref() -> ?SUCHTHAT(R, struct_ref1(),
                          begin
                              #struct_ref{ dsize=D, psize=P } = R,
                              D + P > 0
                          end).

struct_ref1() -> ?SIZED(D, struct_ref1(D)).
struct_ref1(D) -> ?SIZED(P, struct_ref1(D, P)).
struct_ref1(D, P) -> #struct_ref{ dsize=D, psize=P }.

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
                [?LET(R, struct_ref(), R)
                ]).

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


%% ----------------------------------------
data(Data) ->
    ecapnp_data:new(#msg{ data=Data, %%schema=test_schema(),
                          alloc=[size(S) || S <- Data] }).

data(Data, Ptr) ->
    data([<<0:32, Data:16/integer-little, Ptr:16/integer-little,
            0:Data/integer-unit:64, 0:Ptr/integer-unit:64>>]).
