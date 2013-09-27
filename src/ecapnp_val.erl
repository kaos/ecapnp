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

-module(ecapnp_val).
-author("Andreas Stenius <kaos@astekk.se>").

-export([set/2, set/3, get/2, get/3, size/1]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

set(ValueType, Value) -> set(ValueType, Value, 0).
set(ValueType, Value, Default) ->
    value(set, {ValueType, 
                to_value(ValueType, Value),
                to_value(ValueType, Default)}).

get(ValueType, Data) -> get(ValueType, Data, 0).
get(ValueType, Data, Default) ->
    from_value(ValueType,
               value(get, {ValueType,
                           Data,
                           to_value(ValueType, Default)
                          })).

size(ValueType) ->
    value(size, ValueType).


%% ===================================================================
%% internal functions
%% ===================================================================

-define(DEFINE_TYPE(ValueType, Size, TypeSpec, ),
        value(size, ValueType) -> Size;
        value(get, {ValueType, Data, Default}) ->
               PadSize = 7 - ((Size + 7) rem 8),
               <<Value:Size/TypeSpec, _/bits>>
                   = apply_default(
                       <<Data/bits, 0:PadSize/integer>>,
                       <<Default:Size/TypeSpec, 0:PadSize/integer>>),
               Value;

            value(set, {ValueType, Value, Default}) ->
               PadSize = 7 - ((Size + 7) rem 8),
               <<Data:Size/bits, _/bits>>
                   = apply_default(
                       <<Value:Size/TypeSpec, 0:PadSize/integer>>,
                       <<Default:Size/TypeSpec, 0:PadSize/integer>>),
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
?DEFINE_TYPE(float32, 32, float-little);
?DEFINE_TYPE(float64, 64, float-little).

from_value(bool, <<0:1>>) -> false;
from_value(bool, <<1:1>>) -> true;
from_value(_, Value) -> Value.

to_value(bool, true) -> <<1:1>>;
to_value(bool, _) -> <<0:1>>;
to_value(_, Value) when is_number(Value) -> Value;
to_value(_, _) -> 0.

apply_default(Value, Default) ->
    crypto:exor(Value, Default).
