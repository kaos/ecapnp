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
    #msg{ alloc=[Alloc], data=[<<Data:64/binary-unit:9, _/binary>>]} = ecapnp_data:get_message((Root#object.ref)#ref.data),
    ?assertEqual(9, Alloc),
    ?assertEqual(
       <<0:32/integer-little, 2:16/integer-little,
         6:16/integer-little, 0:64/integer-unit:8>>,
      Data).

-endif.
