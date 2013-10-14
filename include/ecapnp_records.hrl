
%% Schema meta data types, to be renamed to schema_file

-record(schema, {
          node :: ecapnp:schema_node(),
          types=[] :: ecapnp:node_types()
         }).

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

%% Schema Node types, to be renamed with a schema_ prefix

%% TODO: turn the node vs schema node types inside out, so they are the same way that the ref's are in relation to the ref kind.

-record(node, {
          name :: ecapnp:type_name(),
          id=0 :: ecapnp:type_id(),
          source = <<>> :: binary()
         }).

-record(struct, {
          node :: ecapnp:schema_node(),
          dsize=0 :: integer(),
          psize=0 :: integer(),
          esize=inlineComposite :: ecapnp:element_size(),
          union_field=none :: #data{} | none,
          fields=[] :: ecapnp:object_fields(),
          types=[] :: ecapnp:node_types()
         }).

-record(enum, {
          node :: ecapnp:schema_node(),
          values=[] :: list(),
          types=[] :: ecapnp:node_types()
         }).

-record(interface, {
          node :: ecapnp:schema_node(),
          methods=[] :: list()
         }).

-record(const, {
          node :: ecapnp:schema_node(),
          type=0 :: integer(),
          value :: any()
         }).

-record(annotation, {
          node :: ecapnp:schema_node()
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
          type=object :: ecapnp:node_type() | object
         }).

%% For internal use, deprecated
-record(struct_ptr, { offset, dsize, psize, object }).
-record(list_ptr, { offset, size, count, object }).

%% Internal message struct for the data server
-record(msg, {
          schema :: ecapnp:schema(),
          alloc = [] :: list(integer()),
          data = [] :: ecapnp:message()
         }).
