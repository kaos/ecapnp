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

field_test() ->
    AllDefaults = [<<0,0,0,0, 1,0,2,0, %% struct, 2 word data, 4 pointers
                     %% data
                     0: 8/integer, %% intField
                     0: 8/integer, %% groupField.a / boolField
                     0:16/integer, %% union tag
                     0: 8/integer, %% groupField.b
                     0: 8/integer, %% groupField.c

                     0: 1/integer, %% opts.bool
                     0:15/integer, %% padding
                     0:16/integer, %% opts union tag

                     0:16/integer, %% meta.id

                     %% pointers
                     0:64/integer, %% textField
                     0:64/integer, %% opts 0:text, 1:data, 2:object
                     0:64/integer, %% meta.tag
                     0:64/integer, %% meta.data
                     0:64/integer  %% structField
                   >>],
    {ok, Root} = test(root, 'Test', AllDefaults),
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
    ?assertEqual(<<"simple message">>, test(get, message, Struct)),
    ?assertEqual(123, test(get, value, Struct)).

