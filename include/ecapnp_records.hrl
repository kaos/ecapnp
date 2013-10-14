
%% the new schema node record
-record(schema_node, {
          name :: ecapnp:type_name(),
          id=0 :: ecapnp:type_id(),
          src = <<>> :: ecapnp:text(),
          kind :: ecapnp:schema_kind(),
          nodes=[] :: ecapnp:schema_nodes()
         }).

%% obsolete
%% -record(schema, {
%%           node :: ecapnp:schema_node(),
%%           types=[] :: ecapnp:node_types()
%%          }).


%% Field types

-record(ptr, {
          type :: term(),
          idx=0 :: integer(),
          default=null :: ecapnp:value()
         }).

-record(data, {
          type :: term(),
          align=0 :: integer(),
          default=0 :: ecapnp:value()
         }).

-record(group, {
          id=0 :: integer()
         }).


%% obsolete
%% -record(node, {
%%           name :: ecapnp:type_name(),
%%           id=0 :: ecapnp:type_id(),
%%           source = <<>> :: binary()
%%          }).


-record(struct, {
          dsize=0 :: integer(),
          psize=0 :: integer(),
          esize=inlineComposite :: ecapnp:element_size(),
          union_field=none :: #data{} | none,
          fields=[] :: ecapnp:object_fields()

          %% obsolete
          %% types=[] :: ecapnp:node_types(),
          %% node :: ecapnp:schema_node()
         }).

-record(enum, {
          values=[] :: list()
          
          %% obsolete
          %% node :: ecapnp:schema_node(),
          %% types=[] :: ecapnp:node_types()
         }).

-record(interface, {
          extends=[] :: list(),
          methods=[] :: list()

          %% obsolete
          %% node :: ecapnp:schema_node()
         }).

-record(const, {
          type=0 :: integer(),
          value :: any()

          %% obsolete
          %% node :: ecapnp:schema_node()
         }).

-record(annotation, {
          type=0 :: integer(),
          targets=[] :: list(boolean())

          %% obsolete
          %% node :: ecapnp:schema_node()
         }).


%% Runtime data

-record(ref, {
          segment :: ecapnp:segment_id(),
          pos=0 :: integer(),
          offset=0 :: integer(),
          kind=null :: ecapnp:ref_kind(),
          data :: pid()
         }).

-record(struct_ref, {
          dsize=0 :: integer(),
          psize=0 :: integer()
         }).

-record(list_ref, {
          size=empty :: ecapnp:element_size(),
          count=0 :: integer()
         }).

-record(far_ref, {
          segment=0 :: integer(),
          double_far=false :: boolean()
         }).

-record(object, {
          ref :: #ref{},
          schema=object :: ecapnp:schema_node() | object

          %% obsolete
          %type=object :: ecapnp:node_type() | object
         }).

%% Internal message struct for the data server
-record(msg, {
          schema :: ecapnp:schema(),
          alloc = [] :: list(integer()),
          data = [] :: ecapnp:message()
         }).
