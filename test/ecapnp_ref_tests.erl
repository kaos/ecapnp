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

-module(ecapnp_ref_tests).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("include/ecapnp.hrl").

-import(ecapnp_test_utils, [data/1]).

get_test() ->
    Data = data([<<4,0,0,0,2,0,3,0>>]),
    ?assertEqual(
       #ref{ segment=0, pos=0, offset=1, data=Data,
             kind=#struct_ref{ dsize=2, psize=3 } },
       ecapnp_ref:get(0, 0, Data)).

read_struct_ptr_test() ->
    Data = ecapnp_data:new(#msg{ data = [<<0,0,0,0, 0,0, 1,0,  41,0,0,0, 23,0,0,0>>] }),
    Ref = ecapnp_ref:get(0, 0, Data),
    ?assertEqual(
       #ref{ segment=0, pos=1, offset=10,
             kind=#list_ref{ size=inlineComposite, count=2 },
             data=Data },
       ecapnp_ref:read_struct_ptr(0, Ref)).

read_struct_data_test() ->
    Data = ecapnp_data:new(#msg{ data = [<<0,0,0,0, 1,0, 0,0,  1,2,3,4, 5,6,7,8>>] }),
    Ref = ecapnp_ref:get(0, 0, Data),
    ?assertEqual(
       <<5, 6>>,
       ecapnp_ref:read_struct_data(32, 16, Ref)).

read_composite_list_test() ->
    Data = ecapnp_data:new(#msg{ data = [<<1,0,0,0, 55,0,0,0,
                                           8,0,0,0, 1,0, 2,0,
                                           0:(6*64)/integer
                                         >>] }),
    Ref = ecapnp_ref:get(0, 0, Data),
    ?assertEqual(
       [#ref{ segment=0, pos=-1, offset=2, data=Data,
              kind=#struct_ref{ dsize=1, psize=2 } },
        #ref{ segment=0, pos=-1, offset=5, data=Data,
              kind=#struct_ref{ dsize=1, psize=2 } }],
       ecapnp_ref:read_list(Ref)).

read_pointer_list_test() ->
    Data = ecapnp_data:new(
             #msg{
                data = [<<1,0,0,0, 22,0,0,0,
                          5,0,0,0, 34,0,0,0,
                          5,0,0,0, 34,0,0,0,
                          102,111,111,0,0,0,0,0,
                          98,97,114,0,0,0,0,0
                        >>]}),
    ListRef = ecapnp_ref:get(0, 0, Data),
    ?assertEqual(
       #ref{ segment=0, pos=0, offset=0, data=Data,
             kind=#list_ref{ size=pointer, count=2 } },
       ListRef),
    List = ecapnp_ref:read_list(ListRef),
    ?assertEqual(
      [#ref{ segment=0, pos=1, offset=1, data=Data,
             kind=#list_ref{ size=byte, count=4 } },
       #ref{ segment=0, pos=2, offset=1, data=Data,
             kind=#list_ref{ size=byte, count=4 } }],
       List),
    ?assertEqual(
       [<<"foo">>, <<"bar">>],
       [ecapnp_ref:read_text(R) || R <- List]).

read_bool_list_test() ->
    Data = ecapnp_data:new(#msg{ data = [<<1,0,0,0, 81,0,0,0,
                                           129,3,0,0, 0,0,0,0
                                         >>] }),
    Ref = ecapnp_ref:get(0, 0, Data),
    ?assertEqual(
       [<<1:1>>, <<0:1>>, <<0:1>>, <<0:1>>,
        <<0:1>>, <<0:1>>, <<0:1>>, <<1:1>>,
        <<1:1>>, <<1:1>>],
       ecapnp_ref:read_list(Ref)).

read_text_test() ->
    Ref = ecapnp_ref:get(
            0, 0, data(
                    [<<1,0,0,0, 106,0,0,0,
                       "Hello World!", 0,
                       0:24/integer>>] %% <- padding to get whole words
                   )),
    ?assertEqual(<<"Hello World!">>,
                 ecapnp_ref:read_text(Ref)).

read_data_test() ->
    Data = <<123456789:64/integer-little,9876544321:64/integer>>,
    Ref = ecapnp_ref:get(
            0, 0, data(
                    [<<1,0,0,0, 130,0,0,0, Data/binary>>]
                   )),
    ?assertEqual(Data, ecapnp_ref:read_data(Ref)).

follow_far_test() ->    
    Data = data([%% segment 0
                 <<2,0,0,0, 1,0,0,0,
                   0,0,0,0, 0,0,1,0>>,
                 %% segment 1
                 <<0,0,0,0, 0,0,0,0>>,
                 %% segment 2
                 <<6,0,0,0, 0,0,0,0>>
                ]),
    Ref = ecapnp_ref:get(0, 0, Data),
    ?assertEqual(
       #ref{ segment=1, pos=0, offset=0, data=Data, kind=null},
       Ref),
    ?assertEqual(
       #ref{ segment=1, pos=-1, offset=0, data=Data,
             kind=#struct_ref{ dsize=0, psize=1 } },
       ecapnp_ref:get(2, 0, Data)).

copy_test() ->
    Bin = <<0,0,0,0, 2,0,2,0,
            1234:32/integer, 5678:32/integer,
            8765:32/integer, 4321:32/integer,
            0:64/integer,
            1,0,0,0, 106,0,0,0,
            "Hello World!", 0,
            0:24/integer
          >>,
    Data = data([Bin]),
    Ref = ecapnp_ref:get(0, 0, Data),
    ?assertEqual(Bin, ecapnp_ref:copy(Ref)).

copy_struct_list_test() ->
    Bin = <<1,0,0,0, 39,0,0,0,
            8,0,0,0, 1,0,1,0,
            1,2,3,4, 5,6,7,8,
            9,0,0,0, 34,0,0,0,
            0:64/integer-little-unit:2,
            "foo", 0,0:32/integer-little
          >>,
    Data = data([Bin]),
    Ref = ecapnp_ref:get(0, 0, Data),
    ?assertEqual(Bin, ecapnp_ref:copy(Ref)).

copy_struct_list2_test() ->
    Bin = <<1,0,0,0, 55,0,0,0,
            8,0,0,0, 1,0,2,0,

            1,2,3,4, 5,6,7,8,
            17,0,0,0, 34,0,0,0,
            0:64/integer-little,

            8,7,6,5, 4,3,2,1,
            9,0,0,0, 34,0,0,0,
            0:64/integer-little,

            "foo", 0,0:32/integer-little,
            "bar", 0,0:32/integer-little
          >>,
    Data = data([Bin]),
    Ref = ecapnp_ref:get(0, 0, Data),
    ?assertEqual(Bin, ecapnp_ref:copy(Ref)).

alloc_test() ->
    Data = ecapnp_data:new({ecapnp_test_utils:test_schema(), 10}),
    Ref = ecapnp_ref:alloc(0, 5, Data),
    ?assertEqual(
      #ref{ segment=0, pos=0, offset=0, data=Data, kind=null},
      Ref),
    #msg{ alloc=[A], data=[D]} = ecapnp_data:get_message(Data),
    ?assertEqual(5, A),
    ?assertEqual(<<0:10/integer-unit:64>>, D).

set_test() ->
    Data = ecapnp_data:new({ecapnp_test_utils:test_schema(), 10}),
    Kind = #struct_ref{ dsize=3, psize=4 },
    Ref = ecapnp_ref:set(Kind, ecapnp_ref:alloc(0, 5, Data)),
    ?assertEqual(
      #ref{ segment=0, pos=0, offset=0, data=Data,
            kind=Kind},
      Ref),
    #msg{ alloc=[A], data=[D]} = ecapnp_data:get_message(Data),
    ?assertEqual(5, A),
    ?assertEqual(<<0:32/integer,
                   3:16/integer-little,
                   4:16/integer-little,
                   0:9/integer-unit:64>>,
                 D).

write_struct_data_test() ->
    Data = ecapnp_data:new(#msg{ data = [<<0,0,0,0, 1,0, 0,0,  0:64/integer>>] }),
    Ref = ecapnp_ref:get(0, 0, Data),
    ok = ecapnp_ref:write_struct_data(32, 16, <<5, 6>>, Ref),
    #msg{ data=[Bin] } = ecapnp_data:get_message(Data),
    ?assertEqual(
       <<0:32/integer, 1:32/integer-little,
         0:32/integer, 5, 6, 0, 0>>,
       Bin).

write_struct_ptr_test() ->
    Data = ecapnp_data:new(#msg{ data = [<<0,0,0,0, 0,0, 1,0,  1,2,3,4, 5,6,7,8>>] }),
    Ref = ecapnp_ref:get(0, 0, Data),
    Ptr = ecapnp_ref:read_struct_ptr(0, Ref),
    ok = ecapnp_ref:write_struct_ptr(Ptr#ref{ kind=null }, Ref),
    #msg{ data=[Bin] } = ecapnp_data:get_message(Data),
    ?assertEqual(
       <<0:32/integer, 0, 0, 1, 0,
         0:32/integer, 0, 0, 0, 0>>,
       Bin).

write_struct_list_test() ->    
    Data = ecapnp_data:new({ecapnp_test_utils:test_schema(), 10}),
    Kind = #struct_ref{ dsize=1, psize=2 },
    Ref = ecapnp_ref:alloc(Kind, 0, 4, Data),
    _ListRef = ecapnp_ref:alloc_list(0, #list_ref{ size=bit, count=8 }, Ref),
    ok = ecapnp_ref:write_list(0, 0, <<1:1>>, Ref),
    ok = ecapnp_ref:write_list(0, 2, <<0:1>>, Ref),
    ok = ecapnp_ref:write_list(0, 1, <<1:1>>, Ref),
    ok = ecapnp_ref:write_list(0, 4, <<1:1>>, Ref),
    #msg{ alloc=[A], data=[Bin] } = ecapnp_data:get_message(Data),
    ?assertEqual(5, A),
    ?assertEqual(
       <<0,0,0,0, 1,0, 2,0,
         0:64/integer, %% data
         5,0,0,0, 65,0,0,0, %% list ptr (0)
         0:64/integer, %% ptr (1)
         2#00010011, %% 8 bits
         0:56/integer, %% padding
         0:64/integer-unit:5 %% unallocated data
       >>, Bin).

write_composite_list_test() ->
    Data = ecapnp_data:new({ecapnp_test_utils:test_schema(), 20}),
    Kind = #struct_ref{ dsize=1, psize=2 },
    Ref = ecapnp_ref:alloc(Kind, 0, 4, Data),
    ListRef = ecapnp_ref:alloc_list(
                0, #list_ref{ size=#struct_ref{ dsize=2, psize=0 }, count=2 },
                Ref),
    [Ref1, Ref2] = ecapnp_ref:read_list(ListRef),
    ok = ecapnp_ref:write_struct_data(16, 32, <<1,2,3,4>>, Ref1),
    ok = ecapnp_ref:write_struct_data(80, 32, <<5,6,7,8>>, Ref2),
    #msg{ alloc=[A], data=[<<Bin:9/binary-unit:64, _/binary>>] } = ecapnp_data:get_message(Data),
    ?assertEqual(9, A),
    ?assertEqual(
       <<0,0,0,0, 1,0, 2,0,
         0:64/integer, %% data
         5,0,0,0, 39,0,0,0, %% list, off 1, size 7, count 4
         0:64/integer, %% ptr (1)
         8,0,0,0, 2,0,0,0, %% tag, 2 elems a 2 words data 0 ptrs
         0,0,1,2, 3,4,0,0,
         0,0,0,0, 0,0,0,0,
         0,0,0,0, 0,0,0,0,
         0,0,5,6, 7,8,0,0
       >>, Bin).


-endif.
