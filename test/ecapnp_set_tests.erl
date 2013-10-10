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

-module(ecapnp_set_tests).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test/test.capnp.hrl").

root_test() ->
    {ok, Root} = ecapnp_set:root('Test', test(schema)),
    #msg{ alloc=[Alloc], data=[<<Data:9/binary-unit:64, _/binary>>]}
        = ecapnp_data:get_message((Root#object.ref)#ref.data),
    ?assertEqual(9, Alloc),
    ?assertEqual(
       <<0:32/integer-little, 2:16/integer-little,
         6:16/integer-little, 0:8/integer-unit:64>>,
      Data).

data_field_test() ->
    {ok, Root} = ecapnp_set:root('Test', test(schema)),
    ok = test(set, intField, 0, Root),
    #msg{ alloc=[Alloc], data=[<<Data:9/binary-unit:64, _/binary>>]} = ecapnp_data:get_message((Root#object.ref)#ref.data),
    ?assertEqual(9, Alloc),
    ?assertEqual(
       <<0:32/integer-little, 2:16/integer-little, 6:16/integer-little, 
         %% data
         33: 8/integer-little, %% intField
         
         0:24/integer-little,
         0:32/integer-little,

         0:64/integer-little,

         %% pointers
         0:6/integer-unit:64
       >>, Data).

text_field_test() ->
    {ok, Root} = ecapnp_set:root('Test', test(schema)),
    ok = test(set, textField, <<"test data">>, Root),
    #msg{ alloc=[Alloc], data=[<<Data:11/binary-unit:64, _/binary>>]} = ecapnp_data:get_message((Root#object.ref)#ref.data),
    ?assertEqual(11, Alloc),
    ?assertEqual(
       <<0:32/integer-little, 2:16/integer-little, 6:16/integer-little, 
         %% data
         0: 8/integer-little, %% intField
         
         0:24/integer-little,
         0:32/integer-little,

         0:64/integer-little,

         %% pointers
         21,0,0,0, 82,0,0,0, %% textField ptr
         0:5/integer-unit:64,

         %% textField data
         "test data", 0,
         0:6/integer-unit:8
       >>, Data).

list_field_test() ->
    {ok, Root} = ecapnp_set:root('ListTest', test(schema)),
    ?assertEqual([0,0,0], test(set, listInts, 3, Root)),
    ok = test(set, listInts, {1, 222}, Root),
    ok = test(set, listInts, {0, 111}, Root),
    ok = test(set, listInts, {2, -333}, Root),
    #msg{ alloc=[Alloc], data=[<<Data:6/binary-unit:64, _/binary>>]} = ecapnp_data:get_message((Root#object.ref)#ref.data),
    ?assertEqual(6, Alloc),
    ?assertEqual(
       <<0,0,0,0, 0,0,3,0, %% struct ref off 0, 0 data, 3 ptrs
         %% pointers
         9,0,0,0,28,0,0,0, %% listInts: off 1, 3 elems a 4 bytes
         0:64/integer-little, %% listAny: null
         0:64/integer-little, %% listSimples

         111:32/integer-little,
         222:32/integer-little,
         -333:32/integer-little,
         0:32/integer-little %% padding
       >>, Data).

object_field_test() ->
    {ok, Root} = ecapnp_set:root('ListTest', test(schema)),
    ?assertEqual([false, false], test(set, listAny, {{list, bool}, 2}, Root)),
    ok = test(set, listAny, {{list, bool}, {1, true}}, Root),
    ok = test(set, listAny, {{list, bool}, {0, false}}, Root),
    #msg{ alloc=[Alloc], data=[<<Data:5/binary-unit:64, _/binary>>]} = ecapnp_data:get_message((Root#object.ref)#ref.data),
    ?assertEqual(5, Alloc),
    ?assertEqual(
       <<0,0,0,0, 0,0,3,0, %% struct ref off 0, 0 data, 3 ptrs
         %% pointers
         0:64/integer-little, %% listInts: null
         5,0,0,0,17,0,0,0, %% listAny: off 1, 2 elems a 1 bits each
         0:64/integer-little, %% listSimples

         2#00000010,
         0:56/integer-little %% padding
       >>, Data).

object_as_struct_test() ->
    {ok, Root} = ecapnp_set:root('ListTest', test(schema)),
    {ok, Obj} = test(set, listAny, 'Simple', Root),
    ok = test(set, simpleMessage, <<"object text">>, Obj),
    #msg{ alloc=[Alloc], data=[<<Data:9/binary-unit:64, _/binary>>]} = ecapnp_data:get_message((Root#object.ref)#ref.data),
    ?assertEqual(9, Alloc),
    ?assertEqual(
       <<0,0,0,0, 0,0,3,0, %% struct ref off 0, 0 data, 3 ptrs
         %% pointers
         0:64/integer-little, %% listInts: null
         4,0,0,0, 1,0,2,0, %% listAny: 'Simple' struct
         0:64/integer-little, %% listSimples: null

         %% Simple struct (listAny)
         0:64/integer-little, %% data
         0:64/integer-little, %% message
         1,0,0,0, 98,0,0,0, %% ref to 12 bytes of text

         "object text", 0,
         0:32/integer-little %% padding
       >>, Data).

struct_list_test() ->
    {ok, Root} = ecapnp_set:root('ListTest', test(schema)),
    [R1, R2] = test(set, listSimples, 2, Root),
    ok = test(set, value, 332211, R1),
    ok = test(set, defaultValue, 112233, R2),
    #msg{ alloc=[Alloc], data=[<<Data:11/binary-unit:64, _/binary>>]}
        = ecapnp_data:get_message((Root#object.ref)#ref.data),
    ?assertEqual(11, Alloc),
    ?assertEqual(
      <<0,0,0,0, 0,0,3,0,
        0:2/integer-little-unit:64,
        1,0,0,0, 55,0,0,0,
        8,0,0,0, 1,0,2,0,
        (222 bxor 332211):32/integer-little,
        0:32/integer-little,
        0:2/integer-little-unit:64,
        0:32/integer-little,
        (333 bxor 112233):32/integer-little,
        0:2/integer-little-unit:64
      >>, Data).

-endif.
