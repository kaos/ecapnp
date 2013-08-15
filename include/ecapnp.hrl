
-record(schema, {
          id :: atom(),
          source = <<>> :: binary(),
          types=[] :: schema_types()
         }).

-record(struct, {
          id :: atom(),
          source = <<>> :: binary(),
          name :: atom(), 
          dsize=0 :: integer(),
          psize=0 :: integer(),
          fields=[] :: list(),
          types=[] :: schema_types()
         }).

-record(enum, {
          values=[] :: list()
         }).

-record(ptr, {
          type :: term(),
          idx :: integer()
         }).

-record(data, {
          type :: term(),
          align :: integer()
         }).

-record(object, {
          doffset=1 :: integer(),
          poffset :: integer(),
          type :: #struct{},
          schema :: #schema{},
          segment :: binary(),
          segments :: list(binary()),
          parent :: #object{} | #schema{}
         }).

-type schema_type() :: #struct{} | #enum{}.
-type schema_types() :: list({atom(), schema_type()}).
