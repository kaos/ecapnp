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
    ?assertEqual(false, ecapnp_val:get(bool, <<0:1>>, <<0:1>>)),
    ?assertEqual(true, ecapnp_val:get(bool, <<1:1>>, <<0:1>>)),
    ?assertEqual(true, ecapnp_val:get(bool, <<0:1>>, <<1:1>>)),
    ?assertEqual(false, ecapnp_val:get(bool, <<1:1>>, <<1:1>>)).

set_test() ->
    ?assertEqual(<<0:1>>, ecapnp_val:set(bool, false, <<0:1>>)),
    ?assertEqual(<<1:1>>, ecapnp_val:set(bool, true, <<0:1>>)),
    ?assertEqual(<<1:1>>, ecapnp_val:set(bool, false, <<1:1>>)),
    ?assertEqual(<<0:1>>, ecapnp_val:set(bool, true, <<1:1>>)).

inf_nan_32_test() ->
    Pinf = <<0,0,128,127>>,
    Ninf = <<0,0,128,255>>,
    NaN = <<0,0,192,127>>,
    ?assertEqual(Pinf, ecapnp_val:set(float32, inf)),
    ?assertEqual(Ninf, ecapnp_val:set(float32, '-inf')),
    ?assertEqual(NaN, ecapnp_val:set(float32, nan)),
    ?assertEqual(inf, ecapnp_val:get(float32, Pinf)),
    ?assertEqual('-inf', ecapnp_val:get(float32, Ninf)),
    ?assertEqual(nan, ecapnp_val:get(float32, NaN)).
    
inf_nan_64_test() ->
    Pinf = <<0:32, 0,0,240,127>>,
    Ninf = <<0:32, 0,0,240,255>>,
    NaN = <<0:32, 0,0,248,127>>,
    ?assertEqual(Pinf, ecapnp_val:set(float64, inf)),
    ?assertEqual(Ninf, ecapnp_val:set(float64, '-inf')),
    ?assertEqual(NaN, ecapnp_val:set(float64, nan)),
    ?assertEqual(inf, ecapnp_val:get(float64, Pinf)),
    ?assertEqual('-inf', ecapnp_val:get(float64, Ninf)),
    ?assertEqual(nan, ecapnp_val:get(float64, NaN)).
    
float_test() ->
    F = 1234.5,
    F32 = ecapnp_val:set(float32, F),
    F64 = ecapnp_val:set(float64, F),
    ?assertEqual(4, size(F32)),
    ?assertEqual(8, size(F64)),
    ?assertEqual(F, ecapnp_val:get(float32, F32)),
    ?assertEqual(F, ecapnp_val:get(float64, F64)).

-endif.
