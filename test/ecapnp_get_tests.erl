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
                type=T }, Root).

field_defaults_test() ->
    Msg= [<<0,0,0,0, 2,0,4,0, %% struct, 2 word data, 4 pointers
            %% data
            0:64/integer-unit:2,
            %% pointers
            0:64/integer-unit:4
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
    Msg = [<<0,0,0,0, 2,0,5,0, %% struct, 2 word data, 5 pointers
             %% data
             33: 8/integer-little, %% intField
             0: 8/integer-little, %% groupField.a / boolField
             1:16/integer-little, %% union tag
             %% 32
             55: 8/integer-little, %% groupField.b
             22: 8/integer-little, %% groupField.c

             0: 1/integer-little, %% opts.bool
             0:15/integer-little, %% padding
             %% 64
             3:16/integer-little, %% opts union tag
             1234:16/integer-little, %% meta.id
             %% 96
             0:32/integer-little, %% padding
             %% 128

             %% pointers
             1:32/integer-little, 2:32/integer-little, %% textField
             %% opts 2:object
             12:32/integer-little,
             1:16/integer-little,
             2:16/integer-little,
             0:64/integer-little, %% meta.tag
             0:64/integer-little, %% meta.data
             %% structField (offset -1, 0 data 0 ptrs), effectively a null ptr
             %% but doesn't use the default values defined for this field
             -4:32/integer-little, 0:32/integer-little,
             %% opts.object data
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
