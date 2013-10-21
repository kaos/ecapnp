
%% Common record for all schema nodes
-record(schema_node, {
          name :: ecapnp:type_name(),
          id=0 :: ecapnp:type_id(),
          src = <<>> :: ecapnp:text(),
          kind=file :: ecapnp:schema_kind(),
          annotations=[] :: list(),
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
          methods=[] :: list(),
          
          %% implementation specifics
          struct :: ecapnp:struct()
         }).

%% Const node
-record(const, {
          field
         }).

%% Annotation node
-record(annotation, {
          type,
          targets=[] :: list(atom())
         }).

%% Struct field
-record(field, {
          name,
          kind,
          annotations=[]
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

%% Interface methods
-record(method, {
          name,
          paramType,
          resultType
         }).


%% Runtime data

-record(ref, {
          segment :: ecapnp:segment_id(),
          pos=-1 :: ecapnp:segment_pos(),
          offset=0 :: ecapnp:segment_offset(),
          kind=null :: ecapnp:ref_kind(),
          data :: pid()
         }).

-define(struct_ref_data,
        %% DON'T TOUCH
        dsize=0 :: ecapnp:word_count(),
        psize=0 :: ecapnp:ptr_count()
                   ).

-record(struct_ref, {
          ?struct_ref_data
         }).

-record(list_ref, {
          size=empty :: ecapnp:element_size(),
          count=0 :: non_neg_integer()
         }).

-record(far_ref, {
          segment=0 :: non_neg_integer(),
          double_far=false :: boolean()
         }).

-record(interface_ref, {
          ?struct_ref_data
         }).

-record(object, {
          ref=null :: ecapnp:ref(),
          schema=object :: object | ecapnp:schema_node()
         }).

-undef(struct_ref_data).

%% Capability & RPC

-record(request, {
          method :: ecapnp:field_name(),
          param :: ecapnp:object(),
          interface :: ecapnp:schema_node()
         }).

%% Internal message struct for the data server
-record(msg, {
          schema :: ecapnp:schema(),
          alloc=[] :: list(integer()),
          data=[] :: ecapnp:message()
         }).
