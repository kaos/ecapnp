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
-export([data/1, test_schema/0]).

%% ----------------------------------------
data(Data) ->
    ecapnp_data:new(#msg{ data=Data, schema=test_schema(),
                          alloc=[size(S) || S <- Data] }).

%% ----------------------------------------
test_schema() ->
    [#schema_node{ %% 0xa3ae19d80d2cfc86
        name=test, id=11794392889854590086, src= <<"test.capnp">>,
        kind=file,
        nodes=
            [#schema_node{ %% 0xb96835191105fedf
                name='Embedd', id=13359986676370636511, src= <<"test.capnp:Embedd">>,
                kind=#struct{ 
                        dsize=1, psize=0, esize=fourBytes,
                        union_field=none,
                        fields=
                            [#field{ name=value,
                                     kind=#data{ type=int32, align=0,
                                     default= <<4,3,2,255>> }}
                            ]}},
             #schema_node{ %% 0x91bf2daf591a2be2
                name='Test', id=10502163087188700130, src= <<"test.capnp:Test">>,
                kind=#struct{
                        dsize=1, psize=2, esize=inlineComposite,
                        union_field=#data{
                                       align=0, type=
                                           {union,
                                            [{foo,
                                              #ptr{ type=text, idx=0,
                                                    default= <<"foo">> }},
                                             {bar,
                                              #ptr{ type=text, idx=0,
                                                    default= <<"bar">> }}
                                            ]} },
                        fields=
                            [#field{ name=boolField,
                                     kind=#data{ type=bool, align=23,
                                                 default= <<1:1>> }},
                             #field{ name=intField,
                                     kind=#data{ type=uint32, align=32,
                                                 default= <<1,2,3,4>> }},
                             #field{ name=structField,
                                     kind=#ptr{ type={struct,13359986676370636511}, idx=1,
                                                default= <<0,0,0,0,1,0,0,0,199,207,255,255,0,0,0,0>> }}
                            ]}}
            ]}].

-endif.
