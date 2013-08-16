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

-module(ecapnp_serialize).
-author("Andreas Stenius <kaos@astekk.se>").

-export([unpack/1]).

%% ===================================================================
%% API functions
%% ===================================================================

unpack(Data)
  when is_binary(Data) ->
    unpack_data(Data).

%% ===================================================================
%% internal functions
%% ===================================================================

unpack_data(Data) -> unpack_data(Data, <<>>).

unpack_data(<<0, Count:8/integer, Rest/binary>>, Acc) ->
    Size = Count + 1,
    unpack_data(Rest, <<Acc/binary, 0:Size/integer-unit:64>>);
unpack_data(<<16#ff, Count:8/integer, UnpackedCount:8/integer, Rest0/binary>>, Acc) ->
    Zeros = Count + 1,
    Words = UnpackedCount + 1,
    <<Unpacked:Words/binary-unit:64, Rest/binary>> = Rest0,
    unpack_data(Rest, <<Acc/binary, 0:Zeros/integer-unit:64, Unpacked/binary>>);
unpack_data(<<Tag:8/integer, Rest/binary>>, Acc) ->
    unpack_tag([(Tag bsr B) band 1 || B <- lists:seq(0, 7)], Rest, Acc);
unpack_data(<<>>, Acc) -> Acc.

unpack_tag([0|Bs], Rest, Acc) ->
    unpack_tag(Bs, Rest, <<Acc/binary, 0>>);
unpack_tag([1|Bs], <<B:1/binary, Rest/binary>>, Acc) ->
    unpack_tag(Bs, Rest, <<Acc/binary, B/binary>>);
unpack_tag([], Rest, Acc) ->
    unpack_data(Rest, Acc).

