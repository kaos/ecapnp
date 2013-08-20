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

-module(ecapnp_data).
-author("Andreas Stenius <kaos@astekk.se>").

-export([new/1, alloc/3, update_segment/4, get_segment/4]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================


new(Init) ->
    spawn_link(fun() -> data_state(Init) end).

alloc(Id, Size, Pid) 
  when is_integer(Id), is_integer(Size) ->
    data_request(alloc, {Id, Size}, Pid).
    
update_segment(Id, Offset, Data, Pid)
  when is_integer(Id), is_integer(Offset), is_binary(Data) ->
    data_request(update_segment, {Id, Offset, Data}, Pid).

get_segment(Id, Offset, Length, Pid)
  when is_integer(Id), is_integer(Offset) andalso
       is_integer(Length); Length == all ->
    data_request(get_segment, {Id, Offset, Length}, Pid).


%% update_data(Data, Offset, Obj) ->
%%     update_segment(
%%       segment_id(Obj),
%%       Obj#object.doffset + Offset,
%%       Data, Obj).

%% update_pointer(Data, Offset, Obj) ->
%%     update_segment(
%%       segment_id(Obj),
%%       Obj#object.poffset + Offset,
%%       Data, Obj).



%% ===================================================================
%% internal functions
%% ===================================================================

empty_message(Size) -> [empty_segment(Size)].
empty_segment(Size) -> <<0:Size/integer-unit:64>>.

data_request(Request, Args, Pid) 
  when is_pid(Pid) ->
    Pid ! {self(), Request, Args},
    receive
        {Request, Result} -> Result
    end.

%% ===================================================================
%% Data state functions, should only be called from the data process
%% ===================================================================

data_state(Message0)
  when is_record(Message0, msg) ->
    receive
        {From, Request, Args} ->
            {Rsp, Message} = handle_request(Request, Args, Message0),
            From ! {Request, Rsp},
            data_state(Message)
    end;

data_state(Size) 
  when is_integer(Size) ->
    data_state(#msg{ alloc=[0], data=empty_message(Size)}).


handle_request(alloc, {Id, Size}, Msg) ->
    do_alloc(Id, Size, Msg);
handle_request(update_segment, {Id, Offset, Data}, Msg) ->
    do_update_segment(Id, Offset, Data, Msg);
handle_request(get_segment, {Id, Offset, Length}, Msg) ->
    do_get_segment(Id, Offset, Length, Msg);
handle_request(Req, _Args, Msg) ->
    {{bad_request, Req}, Msg}.

                             
do_alloc([Id|Ids], Size, Msg) ->
    case do_alloc_data(Id, Size, Msg) of
        {false, Msg} -> do_alloc(Ids, Size, Msg);
        Result -> Result
    end;
do_alloc([], _Size, Msg) ->
    {false, Msg}; %% TODO: add new segment
                     
do_alloc(Id, Size, Msg) ->
    case do_alloc_data(Id, Size, Msg) of
        {false, Msg} ->
            do_alloc_data(
              lists:seq(0, segment_count(Msg) - 1) -- [Id],
              Size, Msg);
        Result -> Result
    end.
    
do_alloc_data(Id, Size, #msg{ alloc=Size }=Msg) ->
    Segment = get_segment(Id, Msg),
    SegSize = size(Segment),
    {PreA, [Alloced|PostA]} = lists:split(Id, Size),
    if Size =< (SegSize - Alloced) ->
            {{Id, Alloced}, 
             Msg#msg{ 
               alloc = PreA ++ [Alloced + Size|PostA] 
              }};
       true -> {false, Msg}
    end.

do_update_segment(Id, Offset, Data, Msg) ->
    Size = size(Data),
    <<Pre:Offset/binary-unit:64,
      _:Size/binary,
      Post/binary>> = get_segment(Id, Msg),
    set_segment(
      Id,
      <<Pre/binary,
        Data/binary, 
        Post/binary>>,
      Msg).

do_get_segment(Id, Offset, all, Msg) ->
    <<_:Offset/binary-unit:64,
      Segment/binary>> = get_segment(Id, Msg),
    {Segment, Msg};
do_get_segment(Id, Offset, Length, Msg) ->
    <<_:Offset/binary-unit:64,
      Segment:Length/binary-unit:64,
      _/binary>> = get_segment(Id, Msg),
    {Segment, Msg}.


%% ===================================================================
%% Data utils
%% ===================================================================

get_segment(Id, #msg{ data=Segments }) ->
    lists:nth(Id + 1, Segments).

segment_count(#msg{ alloc=List }) ->
    length(List).

set_segment(Id, Segment, #msg{ data=Segments }=Msg) ->
    {Pre, [_|Post]} = lists:split(Id, Segments),
    Msg#msg{ data = Pre ++ [Segment|Post] }.

