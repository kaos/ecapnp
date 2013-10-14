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

-module(ecapnp_get_tests).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test/test.capnp.hrl").

root_test() ->
    Msg = [<<0,0,0,0, 2,0,3,0>>],
    {ok, Root} = ecapnp_get:root('Test', test(schema), Msg),
    {ok, T} = ecapnp_schema:lookup('Test', test(schema)),
    Data = (Root#object.ref)#ref.data,
    ?assertEqual(
       #object{ ref=#ref{ segment=0, pos=0, offset=0, data=Data,
                          kind=#struct_ref{ dsize=2, psize=3 } },
                schema=T }, Root).

field_defaults_test() ->
    Msg= [<<0,0,0,0, 2,0,6,0, %% struct, 2 word data, 6 pointers
            %% data
            0:64/integer-unit:2,
            %% pointers
            0:64/integer-unit:6
          >>],
    check_default_values(Msg),
    check_default_values([<<0:64/integer>>]).

check_default_values(Msg) ->
    {ok, Root} = test(root, 'Test', Msg),
    ?assertEqual(33, test(get, intField, Root)),
    ?assertEqual(<<"test">>, test(get, textField, Root)),
    ?assertEqual({boolField, false}, test(get, Root)),
    Grp = test(get, opts, Root),
    ?assertEqual({bool, true}, test(get, Grp)),
    Meta = test(get, meta, Root),
    ?assertEqual(0, test(get, id, Meta)),
    ?assertEqual(<<>>, test(get, tag, Meta)),
    ?assertEqual(<<"1234">>, test(get, data, Meta)),
    Struct = test(get, structField, Root),
    ?assertEqual(<<"simple message">>, test(get, simpleMessage, Struct)),
    ?assertEqual(<<"default message">>, test(get, message, Struct)),
    ?assertEqual(222, test(get, value, Struct)),
    ?assertEqual(333, test(get, defaultValue, Struct)).

field_values_test() ->
    Msg = [<<0,0,0,0, 2,0,6,0, %% struct, 2 word data, 6 pointers
             %% data
             33: 8/integer-little, %% intField
             0: 8/integer-little, %% tag 0: boolField, 1: groupField.a
             1:16/integer-little, %% union tag
             %% 32
             55: 8/integer-little, %% tag 1: groupField.b
             22: 8/integer-little, %% tag 1: groupField.c

             0: 1/integer-little, %% opts tag 0: bool
             0:15/integer-little, %% padding
             %% 64
             3:16/integer-little, %% opts union tag
             1234:16/integer-little, %% meta.id
             %% 96
             0:32/integer-little, %% padding
             %% 128

             %% pointers
             1:32/integer-little, 2:32/integer-little, %% textField
             %% opts tag 1: text, 2: data, 3: object
             16:32/integer-little,
             1:16/integer-little,
             2:16/integer-little,

             0:64/integer-little, %% meta.tag
             0:64/integer-little, %% meta.data
             %% structField (offset -1, 0 data 0 ptrs), effectively a null ptr
             %% but doesn't use the default values defined for this field
             -4:32/integer-little, 0:32/integer-little,
             0:64/integer-little, %% meta.struct

             %% opts.object data (Simple struct)
             222:32/integer-little-unsigned, %% value
             0:32/integer-little-unsigned, %% defaultValue
             0:64/integer-little, %% message
             1:32/integer-little, 106:32/integer-little, %% simpleMessage
             "Hello World!", 0, 0:24/integer-little
           >>],
    {ok, Root} = test(root, 'Test', Msg),
    ?assertEqual(0, test(get, intField, Root)),
    ?assertEqual(<<>>, test(get, textField, Root)),
    {groupField, Union} = test(get, Root),
    ?assertEqual(-44, test(get, a, Union)),
    ?assertEqual(0, test(get, b, Union)),
    ?assertEqual(22, test(get, c, Union)),
    Grp = test(get, opts, Root),
    {object, Obj} = test(get, Grp),
    Simple = ecapnp_obj:to_struct('Simple', Obj),
    ?assertEqual(<<"Hello World!">>, test(get, simpleMessage, Simple)),
    ?assertEqual(<<"default message">>, test(get, message, Simple)),
    ?assertEqual(0, test(get, value, Simple)),
    ?assertEqual(333, test(get, defaultValue, Simple)),
    Meta = test(get, meta, Root),
    ?assertEqual(1234, test(get, id, Meta)),
    ?assertEqual(<<>>, test(get, tag, Meta)),
    ?assertEqual(<<"1234">>, test(get, data, Meta)),
    Struct = test(get, structField, Root),
    ?assertEqual(<<"simple message">>, test(get, simpleMessage, Struct)),
    ?assertEqual(<<"default message">>, test(get, message, Struct)),
    ?assertEqual(222, test(get, value, Struct)),
    ?assertEqual(333, test(get, defaultValue, Struct)).


default_list_test() ->
    {ok, Root} = test(root, 'ListTest', [<<0:64/integer>>]),
    ?assertEqual([456, 789, -123], test(get, listInts, Root)),
    ?assertEqual([], ecapnp_obj:to_list(bool, test(get, listAny, Root))),
    [Obj1, Obj2] = test(get, listSimples, Root),
    [?assertEqual(E, test(get, F, O))
     || {O, T} <- [{Obj1, [{value, 1}, {message, <<"first">>}]},
                   {Obj2, [{value, 2}, {message, <<"second">>}]}],
        {F, E} <- T].

text_list_test() ->
    Text1 = <<"abcdefghijklmnopqrstuvwxyz">>,
    Text2 = <<"0123456789">>,
    Text3 = <<"The end">>,
    Msg = <<0,0,0,0, 0,0,4,0,
            0:3/integer-little-unit:64,
            1,0,0,0, 30,0,0,0, %% listText: off 0, 3 ptrs
            9,0,0,0, 218,0,0,0, %% text 1, 26 bytes+NULL, 4 words
            21,0,0,0, 90,0,0,0, %% text 2, 10 bytes+NULL, 2 words
            25,0,0,0, 66,0,0,0, %% text 3, 7+NULL, 1 word
            Text1/binary, 0,
            0:5/integer-little-unit:8, %% padding
            Text2/binary, 0,
            0:5/integer-little-unit:8, %% padding
            Text3/binary, 0>>,
    {ok, Root} = test(root, 'ListTest', [Msg]),
    [?assertEqual(Expect, Actual)
     || {Expect, Actual} <- 
            lists:zip(
              [Text1, Text2, Text3],
              test(get, listText, Root))].

-endif.
