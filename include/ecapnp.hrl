
%% Schema meta data types

-record(schema, {
          node :: schema_node(),
          types=[] :: schema_types()
         }).

%% Field types

-record(ptr, {
          type :: term(),
          idx=0 :: integer(),
          default :: value()
         }).

-record(data, {
          type :: term(),
          align=0 :: integer(),
          default :: value()
         }).

-record(group, {
          id=0 :: integer()
         }).

%% Node types

-record(node, {
          name :: atom(),
          id=0 :: integer(),
          source = <<>> :: binary()
         }).

-record(struct, {
          node :: schema_node(),
          dsize=0 :: integer(),
          psize=0 :: integer(),
          esize=inlineComposite :: element_size(),
          union_field=none :: #data{} | none,
          fields=[] :: object_fields(),
          types=[] :: schema_types()
         }).

-record(enum, {
          node :: schema_node(),
          values=[] :: list(),
          types=[] :: schema_types()
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

-type schema_node() :: #node{}.
-type schema_type() :: #struct{} | #enum{} | #interface{} | #const{} | #annotation{}.
-type schema_types() :: list({atom(), schema_type()}).
-type object_field() :: #data{} | #ptr{}.
-type object_fields() :: list({atom(), object_field()}).
-type element_size() :: empty | bit | byte | twoBytes | fourBytes | eightBytes | pointer | inlineComposite.
-type value() :: number() | list(value()) | {binary(), list(binary())} | null | undefined.


%% Runtime data


-record(object, {
          type :: schema_node(),
          segment_id=0 :: integer(),
          doffset=0 :: integer(),
          dsize=0 :: integer(),
          poffset=0 :: integer(),
          psize=0 :: integer(),
          union_value :: {atom(), term()} | undefined,
          data :: pid()
         }).

%% For internal use
-record(struct_ptr, { offset, dsize, psize, object }).
-record(list_ptr, { offset, size, count, object }).

-record(msg, {
          schema :: #schema{},
          alloc = [] :: list(integer()),
          data = [] :: list(binary())
         }).
