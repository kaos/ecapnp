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

-export([set/6, get/5]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

set(ValueType, Value, Offset, Align, Default, Segment) ->
    value(set, {{ValueType, Value}, Offset, Align, Default, Segment}).

get(ValueType, Offset, Align, Default, Segment) ->
    value(get, {ValueType, Offset, Align, Default, Segment}).


%% ===================================================================
%% internal functions
%% ===================================================================

-define(DEFINE_TYPE(ValueType, Size, TypeSpec, ),
        value(get, {ValueType, Offset, Align, Default, Segment}) ->
               BinOffset = Offset + (Align div 64),
               BinAlign = Align rem 64,
               Len = ((BinAlign + Size) + 7) div 8,

               <<_:BinOffset/binary-unit:64,
                 Bin:Len/binary, _/binary>> = Segment,

               DefaultValue = to_value(ValueType, Default),
               DefaultPadLen = (Len * 8) - (BinAlign + Size),

               <<_:BinAlign/bits,
                 Value:Size/TypeSpec, _/bits>>
                   = apply_default(Bin,
                                   <<0:BinAlign/integer,
                                     DefaultValue:Size/TypeSpec,
                                     0:DefaultPadLen/integer>>),

               from_value(ValueType, Value);

            value(set, {{ValueType, Value}, Offset, Align, Default, Segment}) ->
               <<Pre:Offset/binary-unit:64,
                 PreV:Align/bits,
                 _:Size/TypeSpec,
                 Post/bits>> = Segment,

               DefaultValue = to_value(ValueType, Default),
               Value1 = to_value(ValueType, Value),
               PadSize = 7 - ((Size + 7) rem 8),
               <<_:PadSize/bits, BinValue/bits>>
                   = apply_default(
                       <<0:PadSize/integer, Value1:Size/TypeSpec>>,
                       <<0:PadSize/integer, DefaultValue:Size/TypeSpec>>),
               <<Pre/binary, PreV/bits, BinValue/bits, Post/bits>>
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
