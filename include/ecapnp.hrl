
%% Schema meta data types, to be renamed to schema_file

-record(schema, {
          node :: schema_node(),
          types=[] :: node_types()
         }).

%% Field types

-record(ptr, {
          type :: term(),
          idx=0 :: integer(),
          default=null :: value()
         }).

-record(data, {
          type :: term(),
          align=0 :: integer(),
          default=0 :: value()
         }).

-record(group, {
          id=0 :: integer()
         }).

%% Schema Node types, to be renamed with a schema_ prefix

%% TODO: turn the node vs schema node types inside out, so they are the same way that the ref's are in relation to the ref kind.

-record(node, {
          name :: type_name(),
          id=0 :: type_id(),
          source = <<>> :: binary()
         }).

-record(struct, {
          node :: schema_node(),
          dsize=0 :: integer(),
          psize=0 :: integer(),
          esize=inlineComposite :: element_size(),
          union_field=none :: #data{} | none,
          fields=[] :: object_fields(),
          types=[] :: node_types()
         }).

-record(enum, {
          node :: schema_node(),
          values=[] :: list(),
          types=[] :: node_types()
         }).

-record(interface, {
          node :: schema_node(),
          methods=[] :: list()
         }).

-record(const, {
          node :: schema_node(),
          type=0 :: integer(),
          value :: any()
         }).

-record(annotation, {
          node :: schema_node()
         }).


%% Runtime data

-record(ref, {
          segment :: segment_id(),
          pos=0 :: integer(),
          offset=0 :: integer(),
          kind=null :: ref_kind(),
          data :: pid()
         }).

-record(struct_ref, {
          dsize=0 :: integer(),
          psize=0 :: integer()
         }).

-record(list_ref, {
          size=empty :: element_size(),
          count=0 :: integer()
         }).

-record(far_ref, {
          segment=0 :: integer(),
          double_far=false :: boolean()
         }).

-record(object, {
          ref :: #ref{},
          type=object :: node_type() | object
         }).

%% For internal use, deprecated
-record(struct_ptr, { offset, dsize, psize, object }).
-record(list_ptr, { offset, size, count, object }).

%% Internal message struct for the data server
-record(msg, {
          schema :: schema(),
          alloc = [] :: list(integer()),
          data = [] :: message()
         }).

-type schema() :: #schema{}.
-type schema_node() :: #node{}.
-type node_type() :: #struct{} | #enum{} | #interface{} | #const{} | #annotation{}.
-type node_types() :: list({atom(), node_type()}).
-type field_name() :: atom().
-type field_value() :: value() | object().
-type object() :: #object{}.
-type object_field() :: #data{} | #ptr{}.
-type object_fields() :: list({field_name(), object_field()}).
-type element_size() :: empty | bit | byte | twoBytes | fourBytes | eightBytes | pointer | inlineComposite.
-type value() :: number() | boolean() | list(value()) | binary() | null.
-type ref_kind() :: null | #struct_ref{} | #list_ref{} | #far_ref{}.
-type segment_id() :: integer().
-type message() :: list(binary()).
-type type_name() :: atom().
-type type_id() :: integer().
-type schema_type() :: type_name() | type_id().
