
%% Common record for all schema nodes
-record(schema_node, {
          name :: ecapnp:type_name(),
          id=0 :: ecapnp:type_id(),
          src = <<>> :: ecapnp:text(),
          kind :: ecapnp:schema_kind(),
          nodes=[] :: ecapnp:schema_nodes()
         }).

%% Struct node
-record(struct, {
          dsize=0 :: ecapnp:word_count(),
          psize=0 :: ecapnp:ptr_count(),
          esize=inlineComposite :: ecapnp:element_size(),
          union_field=none :: none | ecapnp:field_type(),
          fields=[] :: ecapnp:struct_fields()
         }).

%% Enum node
-record(enum, {
          values=[] :: ecapnp:enum_values()
         }).

%% Interface node
-record(interface, {
          extends=[] :: list(),
          methods=[] :: list()
         }).

%% Const node
-record(const, {
          type=0 :: integer(),
          value :: any()
         }).

%% Annotation node
-record(annotation, {
          type=0 :: integer(),
          targets=[] :: list(boolean())
         }).

%% Schema Field types
-record(ptr, {
          type :: term(),
          idx=0 :: ecapnp:ptr_index(),
          default=null :: ecapnp:value()
         }).

-record(data, {
          type :: term(),
          align=0 :: ecapnp:bit_count(),
          default :: ecapnp:value()
         }).

-record(group, {
          id=0 :: ecapnp:type_id()
         }).


%% Runtime data

-record(ref, {
          segment :: ecapnp:segment_id(),
          pos=-1 :: ecapnp:segment_pos(),
          offset=0 :: ecapnp:segment_offset(),
          kind=null :: ecapnp:ref_kind(),
          data :: ecapnp:data()
         }).

-record(struct_ref, {
          dsize=0 :: ecapnp:word_count(),
          psize=0 :: ecapnp:ptr_count()
         }).

-record(list_ref, {
          size=empty :: ecapnp:element_size(),
          count=0 :: non_neg_integer()
         }).

-record(far_ref, {
          segment=0 :: non_neg_integer(),
          double_far=false :: boolean()
         }).

-record(object, {
          ref :: ecapnp:ref(),
          schema=object :: object | ecapnp:schema_node()
         }).

%% Internal message struct for the data server
-record(msg, {
          schema :: ecapnp:schema(),
          alloc = [] :: list(integer()),
          data = [] :: ecapnp:message()
         }).
