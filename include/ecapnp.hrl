
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
          idx :: integer()
         }).

-record(data, {
          type :: term(),
          align :: integer()
         }).

-type schema_type() :: #struct{} | #enum{}.
-type schema_types() :: list({atom(), schema_type()}).

%% Runtime object meta data
-record(object, {
          doffset=1 :: integer(),
          poffset :: integer(),
          type :: #struct{},
          schema :: #schema{},
          segment :: binary(),
          segments :: list(binary()),
          parent :: #object{} | #schema{}
         }).
