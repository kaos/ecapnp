
%% Schema meta data types

-record(schema, {
          name :: atom(),
          id :: integer(),
          source = <<>> :: binary(),
          types=[] :: schema_types()
         }).

%% Field types
-record(ptr, {
          type :: term(),
          idx=0 :: integer()
         }).

-record(data, {
          type :: term(),
          align=0 :: integer()
         }).

-record(group, {
          id :: integer()
         }).

%% Object types
-record(struct, {
          name :: atom(), 
          id :: integer(),
          source = <<>> :: binary(),
          dsize=0 :: integer(),
          psize=0 :: integer(),
          esize=inlineComposite :: element_size(),
          union_field=none :: #data{} | none,
          fields=[] :: object_fields(),
          types=[] :: schema_types()
         }).

-record(enum, {
          name :: atom(),
          id :: integer(),
          source = <<>> :: binary(),
          values=[] :: list(),
          types=[] :: schema_types()
         }).

-type schema_type() :: #struct{} | #enum{}.
-type schema_types() :: list({atom(), schema_type()}).
-type object_field() :: #data{} | #ptr{}.
-type object_fields() :: list({atom(), object_field()}).
-type element_size() :: empty | bit | byte | twoBytes | fourBytes | eightBytes | pointer | inlineComposite.


%% Runtime data

-record(type, {
          name :: atom(),
          id=0 :: integer()
         }).

-record(object, {
          type :: #type{},
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
