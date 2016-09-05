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
%% @reference <a href="http://kentonv.github.io/capnproto/encoding.html#packing">Cap'n
%% Proto packing</a>.
%% @doc Serialize messages.
%%
%% Currently, only the packing scheme is implemented.

-module(ecapnp_serialize).
-author("Andreas Stenius <kaos@astekk.se>").

-export([pack/1, unpack/1]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec pack(binary()) -> binary().
%% @doc Pack message data.
%%
%% Pack the final message data using the <a
%% href="http://kentonv.github.io/capnproto/encoding.html#packing">Cap'n
%% Proto packing</a> scheme.
pack(Data)
  when is_binary(Data) ->
    pack_data(Data).

-spec unpack(binary()) -> binary().
%% @doc Unpack message data.
%%
%% Unpack data using the <a
%% href="http://kentonv.github.io/capnproto/encoding.html#packing">Cap'n
%% Proto packing</a> scheme.
unpack(Data)
  when is_binary(Data) ->
    unpack_data(Data).


%% ===================================================================
%% internal functions
%% ===================================================================

pack_data(Data) -> pack_data(Data, <<>>).

pack_data(<<0:1/integer-unit:64, Rest/binary>>, Acc) ->
    {Count, Rest2} = pack_nulls(Rest),
    pack_data(Rest2, <<Acc/binary, 0, Count:8/integer>>);
pack_data(<<Word:1/binary-unit:64, Rest/binary>>, Acc) ->
    case pack_tag(Word, <<>>) of
        {{8, 255}, TagData} ->
            Count = pack_blob(Rest),
            <<BlobData:Count/binary-unit:64, Rest2/binary>> = Rest,
            pack_data(Rest2, <<Acc/binary, 255, TagData/binary, Count:8/integer, BlobData/binary>>);
        {{_, TagByte}, TagData} ->
            pack_data(Rest, <<Acc/binary, TagByte:8/integer, TagData/binary>>)
    end;
pack_data(<<>>, Acc) -> Acc.

pack_tag(Word, Acc) -> pack_tag([], Word, Acc).

pack_tag(Tag, <<0, Rest/binary>>, Acc) ->
    pack_tag([0|Tag], Rest, Acc);
pack_tag(Tag, <<Byte:1/binary, Rest/binary>>, Acc) ->
    pack_tag([1|Tag], Rest, <<Acc/binary, Byte/binary>>);
pack_tag(Tag, <<>>, Acc) ->
    TagByte = lists:foldl(
                fun(B, {C, V}) ->
                        {C + B, (V bsl 1) bor B}
                end,
                {0, 0},
                Tag),
    {TagByte, Acc}.

pack_blob(Data) -> pack_blob(0, Data).

pack_blob(255, _) ->
  255; % individual blob size can only be stored in 1 byte
pack_blob(Count, <<Word:1/binary-unit:64, Rest/binary>>) ->
    case pack_tag(Word, <<>>) of
        {{C, _V}, _Tag} when C >= 6 ->
            %% keep packing blob as long as we only have 2 or fewer zero bytes in each word
            pack_blob(Count + 1, Rest);
        _ -> Count
    end;
pack_blob(Count, <<>>) -> Count.

pack_nulls(Data) -> pack_nulls(0, Data).

pack_nulls(Count, <<0:1/binary-unit:64, Rest/binary>>) ->
    pack_nulls(Count + 1, Rest);
pack_nulls(Count, Rest) ->
    {Count, Rest}.


unpack_data(Data) -> unpack_data(Data, <<>>).

unpack_data(<<0, Count:8/integer, Rest/binary>>, Acc) ->
    Size = Count + 1,
    unpack_data(Rest, <<Acc/binary, 0:Size/integer-unit:64>>);
unpack_data(<<16#ff, TagData:1/binary-unit:64, Count:8/integer, Rest0/binary>>, Acc) ->
    <<Words:Count/binary-unit:64, Rest/binary>> = Rest0,
    unpack_data(Rest, <<Acc/binary, TagData/binary, Words/binary>>);
unpack_data(<<Tag:8/integer, Rest/binary>>, Acc) ->
    unpack_tag([(Tag bsr B) band 1 || B <- lists:seq(0, 7)], Rest, Acc);
unpack_data(<<>>, Acc) -> Acc.

unpack_tag([0|Bs], Rest, Acc) ->
    unpack_tag(Bs, Rest, <<Acc/binary, 0>>);
unpack_tag([1|Bs], <<B:1/binary, Rest/binary>>, Acc) ->
    unpack_tag(Bs, Rest, <<Acc/binary, B/binary>>);
unpack_tag([], Rest, Acc) ->
    unpack_data(Rest, Acc).
