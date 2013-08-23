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

-module(ecapnp_message).
-author("Andreas Stenius <kaos@astekk.se>").

-export([read/1, write/1]).

-include("ecapnp.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

read(Data)
  when is_binary(Data) ->
    read_message(Data).

write(#object{ data=Pid }) ->
    write_message(ecapnp_data:get_message(Pid)).


%% ===================================================================
%% internal functions
%% ===================================================================

read_message(<<SegCount:32/integer-little, Data/binary>>) ->
    read_message(SegCount+1, SegCount rem 2, Data).

read_message(SegCount, Pad, Data)
  when is_integer(SegCount), SegCount > 0 ->
    <<SegSizes:SegCount/binary-unit:32,
      _Padding:Pad/binary-unit:32,
      Rest/binary>> = Data,
    read_message(SegSizes, Rest, []);
read_message(<<SegSize:32/integer-little, SegSizes/binary>>, Data, Segments) ->
    <<Segment:SegSize/binary-unit:64, Rest/binary>> = Data,
    read_message(SegSizes, Rest, [Segment|Segments]);
read_message(<<>>, <<>>, Segments) ->
    {ok, lists:reverse(Segments)}.

write_message(#msg{ alloc=Alloc, data=Segments }) ->
    SegCount = length(Segments) - 1,
    SegSizes = << <<Size:32/integer-little>> || Size <- Alloc >>,
    Pad = SegCount rem 2,
    Padding = <<0:Pad/integer-unit:32>>,
    Data = << <<Segment:Size/binary-unit:64>>
              || {Segment, Size} <- lists:zip(Segments, Alloc) >>,
    <<SegCount:32/integer-little, SegSizes/binary,
      Padding/binary, Data/binary>>.
