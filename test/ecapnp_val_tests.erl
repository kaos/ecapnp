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

-module(ecapnp_val_tests).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_test() ->
    ?assertEqual(true, ecapnp_val:get(bool, 0, 7, false, <<1:64/integer-little>>)),
    ?assertEqual(true, ecapnp_val:get(bool, 0, 0, true, <<0:64/integer-little>>)).

set_test() ->
    ?assertEqual(<<1:64/integer-little>>, ecapnp_val:set(bool, true, 0, 7, false, <<0:64/integer-little>>)),
    ?assertEqual(<<-2:64/integer-little>>, ecapnp_val:set(bool, true, 0, 7, true, <<-1:64/integer-little>>)).

-endif.
