
%% Schema meta data types

-record(schema, {
          id :: atom(),
          source = <<>> :: binary(),
          name :: atom(),
          types=[] :: schema_types()
         }).

-record(struct, {
          id :: atom(),
          source = <<>> :: binary(),
          name :: atom(), 
          dsize=0 :: integer(),
          psize=0 :: integer(),
          esize=inlineComposite :: element_size(),
          fields=[] :: list(),
          types=[] :: schema_types()
         }).

-record(enum, {
          id :: atom(),
          source = <<>> :: binary(),
          name :: atom(),
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
-record(msg, {
          alloc = [] :: list(integer()),
          data = [] :: list(binary())
         }).

-record(object, {
          segment_id=0 :: integer(),
          doffset=0 :: integer(),
          poffset=0 :: integer(),
          type :: #struct{},
          parent :: #object{} | #schema{},
          msg :: #msg{}
         }).

%% For internal use
-record(list_ptr, { offset, size, count, object }).
