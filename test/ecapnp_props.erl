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

%%% ----------------------------------------
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
           D = ecapnp_data:new(#msg{
                                  data=[<<0:32, Data:16, Ptr:16,
                                          0:Data/integer-unit:64,
                                          0:Ptr/integer-unit:64>>]}),
           Ref = ecapnp_ref:get(0, 0, D),
           ok = ecapnp_ref:write_struct_data(Off, Size, Value, Ref),
           Value =:= ecapnp_ref:read_struct_data(Off, Size, Ref)
       end).
