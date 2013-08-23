
%% Schema meta data types

-record(schema, {
          name :: atom(),
          id :: atom(),
          source = <<>> :: binary(),
          types=[] :: schema_types()
         }).

-record(struct, {
          name :: atom(), 
          id :: atom(),
          source = <<>> :: binary(),
          dsize=0 :: integer(),
          psize=0 :: integer(),
          esize=inlineComposite :: element_size(),
          fields=[] :: list(),
          types=[] :: schema_types()
         }).

-record(enum, {
          name :: atom(),
          id :: atom(),
          source = <<>> :: binary(),
          values=[] :: list(),
          types=[] :: schema_types()
         }).

-record(ptr, {
          type :: term(),
          idx=0 :: integer()
         }).

-record(data, {
          type :: term(),
          align=0 :: integer()
         }).

-type schema_type() :: #struct{} | #enum{}.
-type schema_types() :: list({atom(), schema_type()}).
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
          data :: pid()
         }).

%% For internal use
-record(list_ptr, { offset, size, count, object }).

-record(msg, {
          schema :: #schema{},
          alloc = [] :: list(integer()),
          data = [] :: list(binary())
         }).
