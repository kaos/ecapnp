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

-module(ecapnp).
-author("Andreas Stenius <kaos@astekk.se>").

-export([get_root/3, get/1, get/2]).
-export([set_root/2, set/3]).
%-export([type/1, type/2]).

-include("ecapnp.hrl").
%%-export_type([object/0, field_name/0, value/0]).


%% ===================================================================
%% API functions
%% ===================================================================

get_root(Type, Schema, [Segment|_]=Segments) 
  when is_atom(Type),
       is_record(Schema, schema),
       is_binary(Segment) ->
    ecapnp_get:root(Type, Schema, Segments).

set_root(Type, Schema)
  when is_atom(Type),
       is_record(Schema, schema) ->
    ecapnp_set:root(Type, Schema).

get(Object)
  when is_record(Object, object) ->
    ecapnp_get:union(Object).

get(Field, Object)
  when is_atom(Field), is_record(Object, object) ->
    ecapnp_get:field(Field, Object).

set(Field, Value, Object)
  when is_atom(Field), is_record(Object, object) ->
    ecapnp_set:field(Field, Value, Object).

%% type(#object{ type=T }) ->
%%     T.

%% type(name, Object) ->
%%     (type(Object))#node.name;
%% type(id, Object) ->
%%     (type(Object))#node.id;
%% type(schema, Object) ->
%%     {ok, Schema} = ecapnp_schema:type_of(Object),
%%     Schema.


%% ===================================================================
%% internal functions
%% ===================================================================

%% lookup_field(Name, Object) ->
%%     proplists:get_value(
%%       Name,
%%       (type(schema, Object))#struct.fields,
%%       {unknown_field, Name}).
