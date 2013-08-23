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

-export([new/1, alloc/3, update_segment/3,
         get_segment/4, get_message/1,
         get_type/2]).

-include("ecapnp.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

new(Init) ->
    spawn_link(fun() -> data_state(Init) end).

alloc(Id, Size, Pid) 
  when is_integer(Id), is_integer(Size) ->
    data_request(alloc, {Id, Size}, Pid).
    
update_segment({Id, Offset}, Data, Pid)
  when is_integer(Id), is_integer(Offset), is_binary(Data) ->
    data_request(update_segment, {Id, Offset, Data}, Pid).

get_segment(Id, Offset, Length, Pid)
  when is_integer(Id), is_integer(Offset) andalso
       is_integer(Length); Length == all ->
    data_request(get_segment, {Id, Offset, Length}, Pid).

get_message(Pid) ->
    data_request(get_message, [], Pid).

get_type(Type, Pid)
  when is_atom(Type) ->
    data_request(get_type, Type, Pid).


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

-record(state, { msg, types }).

data_state(State)
  when is_record(State, state) ->
    receive
        {From, Request, Args} ->
            handle_response(
              handle_request(Request, Args, State),
              {Request, From})
    end;

data_state(Message)
  when is_record(Message, msg) ->
    data_state(new_state(Message));
data_state({Schema, MsgSize}) ->
    data_state(new_state(#msg{ 
                            schema=Schema, 
                            alloc=[0],
                            data=empty_message(MsgSize)
                           })).

new_state(#msg{ schema=Schema }=Msg) ->
    #state{
       msg=Msg,
       types=list_types(Schema, [])
      }.

-define(list_types(Type),
        list_types({_, #Type{ types=Ts }}=T, Acc) -> 
               list_types(Ts, [T|Acc]);
            list_types(#Type{ types=Ts }, Acc) ->
               list_types(Ts, Acc)
                   ).
?list_types(schema);
?list_types(struct);
?list_types(enum);
list_types([T|Ts], Acc) ->
    list_types(Ts, list_types(T, Acc));
list_types([], Acc) -> Acc.
-undef(list_types).

handle_response({Response, State}, {Request, From}) ->
    From ! {Request, Response},
    data_state(State);
handle_response(State, {Request, From}) ->
    From ! {Request, ok},
    data_state(State).
    
handle_request(alloc, {Id, Size}, State) ->
    do_alloc(Id, Size, State);
handle_request(update_segment, {Id, Offset, Data}, State) ->
    do_update_segment(Id, Offset, Data, State);
handle_request(get_segment, {Id, Offset, Length}, State) ->
    do_get_segment(Id, Offset, Length, State);
handle_request(get_message, _, State) ->
    {State#state.msg, State};
handle_request(get_type, Type, State) ->
    do_get_type(Type, State);
handle_request(Req, _Args, State) ->
    {{bad_request, Req}, State}.

                             
do_alloc([Id|Ids], Size, State0) ->
    case do_alloc_data(Id, Size, State0) of
        {false, State} -> do_alloc(Ids, Size, State);
        Result -> Result
    end;
do_alloc([], _Size, State) ->
    {false, State}; %% TODO: add new segment
                     
do_alloc(Id, Size, State) ->
    case do_alloc_data(Id, Size, State) of
        {false, State} ->
            do_alloc_data(
              lists:seq(0, segment_count(State) - 1) -- [Id],
              Size, State);
        Result -> Result
    end.
    
do_alloc_data(Id, Size, State) ->
    Segment = get_segment(Id, State),
    SegSize = size(Segment),
    Msg = State#state.msg,
    Alloc = Msg#msg.alloc,
    {PreA, [Alloced|PostA]} = lists:split(Id, Alloc),
    if Size =< (SegSize - Alloced) ->
            {{Id, Alloced}, 
             State#state{
               msg=Msg#msg{ 
                     alloc = PreA ++ [Alloced + Size|PostA] 
                    }}
            };
       true -> {false, State}
    end.

do_update_segment(Id, Offset, Data, State) ->
    Size = size(Data),
    <<Pre:Offset/binary-unit:64,
      _:Size/binary,
      Post/binary>> = get_segment(Id, State),
    set_segment(
      Id,
      <<Pre/binary,
        Data/binary, 
        Post/binary>>,
      State).

do_get_segment(Id, Offset, all, State) ->
    <<_:Offset/binary-unit:64,
      Segment/binary>> = get_segment(Id, State),
    {Segment, State};
do_get_segment(Id, Offset, Length, State) ->
    <<_:Offset/binary-unit:64,
      Segment:Length/binary-unit:64,
      _/binary>> = get_segment(Id, State),
    {Segment, State}.

do_get_type(Type, #state{ types=Ts }=State) ->
    {proplists:get_value(Type, Ts), State}.


%% ===================================================================
%% Data utils
%% ===================================================================

get_segment(Id, #state{ msg=#msg{ data=Segments } }) ->
    lists:nth(Id + 1, Segments).

segment_count(#state{ msg=#msg{ alloc=List } }) ->
    length(List).

set_segment(Id, Segment, 
            #state{
               msg=#msg{
                      data=Segments }=Msg
              }=State) ->
    {Pre, [_|Post]} = lists:split(Id, Segments),
    State#state{ msg=Msg#msg{ data = Pre ++ [Segment|Post] } }.
