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

-module(ecapnp_test_utils).
-ifdef(TEST).
-include("include/ecapnp.hrl").
-export([data/1]).

%% ----------------------------------------
data(Data) ->
    ecapnp_data:new(#msg{ data=Data, schema=test_capnp,
                          alloc=[size(S) || S <- Data] }).

-endif.
