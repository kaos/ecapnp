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
%% @doc Cap'n Proto value support.
%%
%% Everything value.

-module(ecapnp_val).
-author("Andreas Stenius <kaos@astekk.se>").

-export([set/2, set/3, get/2, get/3, size/1]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

-spec set(value_type(), number() | boolean()) -> binary().
%% @doc Encode value to Cap'n Proto format.
set(ValueType, Value) ->
    value(set, {ValueType, to_value(ValueType, Value)}).

-spec set(value_type(), number() | boolean(), binary()) -> binary().
%% @doc Encode value to Cap'n Proto format.
%%
%% The result is XOR'ed with `Default'.
set(ValueType, Value, Default) when is_bitstring(Default) ->
    value(set, {ValueType, to_value(ValueType, Value), Default}).

-spec get(value_type(), binary()) -> number() | boolean().
%% @doc Decode data from Cap'n Proto format.
get(ValueType, Data) when is_bitstring(Data) ->
    from_value(ValueType, value(get, {ValueType, Data})).

-spec get(value_type(), binary(), binary()) -> number() | boolean().
%% @doc Decode data from Cap'n Proto format.
%%
%% The `Data' is XOR'ed with `Default' prior to decoding.
get(ValueType, Data, Default)
  when is_bitstring(Data),
       is_bitstring(Default) ->
    Value = value(get, {ValueType, Data, Default}),
    from_value(ValueType, Value).

-spec size(value_type()) -> non_neg_integer().
%% @doc Get number of bits for `ValueType'.
size(ValueType) ->
    value(size, ValueType).


%% ===================================================================
%% internal functions
%% ===================================================================

-define(DEFINE_TYPE(ValueType, Size, TypeSpec),
        value(size, ValueType) -> Size;
        value(get, {ValueType, Data}) ->
               <<Value:Size/TypeSpec>> = Data, Value;
        value(get, {ValueType, Data, Default}) ->
               PadSize = 7 - ((Size + 7) rem 8),
               <<Value:Size/TypeSpec, _/bits>>
                   = apply_default(
                       <<Data/bits, 0:PadSize/integer>>,
                       <<Default/bits, 0:PadSize/integer>>),
               Value;

            value(set, {ValueType, Value}) ->
               <<Value:Size/TypeSpec>>;
            value(set, {ValueType, Value, Default}) ->
               PadSize = 7 - ((Size + 7) rem 8),
               <<Data:Size/bits, _/bits>>
                   = apply_default(
                       <<Value:Size/TypeSpec, 0:PadSize/integer>>,
                       <<Default/bits, 0:PadSize/integer>>),
               Data
                   ).

?DEFINE_TYPE(uint64, 64, integer-unsigned-little);
?DEFINE_TYPE(uint32, 32, integer-unsigned-little);
?DEFINE_TYPE(uint16, 16, integer-unsigned-little);
?DEFINE_TYPE(uint8, 8, integer-unsigned-little);
?DEFINE_TYPE(int64, 64, integer-signed-little);
?DEFINE_TYPE(int32, 32, integer-signed-little);
?DEFINE_TYPE(int16, 16, integer-signed-little);
?DEFINE_TYPE(int8, 8, integer-signed-little);
?DEFINE_TYPE(bool, 1, bits);
?DEFINE_TYPE(float32, 32, bits); %% actual float conversion is done in the
?DEFINE_TYPE(float64, 64, bits); %% to_value/from_value functions.
value(size, void) -> 0;
value(_, {void, _}) -> void.

-define(INF_NAN_32(N,S), <<0:16,1:1,N:1,0:6,S:1,127:7>>).
-define(INF_NAN_64(N,S), <<0:48,15:4,N:1,0:3,S:1,127:7>>).

from_value(bool, <<0:1>>) -> false;
from_value(bool, <<1:1>>) -> true;
from_value(float32, ?INF_NAN_32(1, _)) -> nan;
from_value(float32, ?INF_NAN_32(0, 0)) -> inf;
from_value(float32, ?INF_NAN_32(0, 1)) -> '-inf';
from_value(float32, <<Value:32/float-little>>) -> Value;
from_value(float64, ?INF_NAN_64(1, _)) -> nan;
from_value(float64, ?INF_NAN_64(0, 0)) -> inf;
from_value(float64, ?INF_NAN_64(0, 1)) -> '-inf';
from_value(float64, <<Value:64/float-little>>) -> Value;
from_value(_, Value) when is_number(Value) -> Value;
from_value(_, void) -> void.

to_value(bool, true) -> <<1:1>>;
to_value(bool, false) -> <<0:1>>;
to_value(float32, inf) -> ?INF_NAN_32(0, 0);
to_value(float32, '-inf') -> ?INF_NAN_32(0, 1);
to_value(float32, nan) -> ?INF_NAN_32(1, 0);
to_value(float32, Value) -> <<Value:32/float-little>>;
to_value(float64, inf) -> ?INF_NAN_64(0, 0);
to_value(float64, '-inf') -> ?INF_NAN_64(0, 1);
to_value(float64, nan) -> ?INF_NAN_64(1, 0);
to_value(float64, Value) -> <<Value:64/float-little>>;
to_value(_, Value) when is_number(Value) -> Value;
to_value(_, void) -> void.

apply_default(Value, Default) ->
    crypto:exor(Value, Default).
