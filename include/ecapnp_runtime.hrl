-record(builder, {
          pid :: pid()
         }).

-record(reader, {
          data :: list(binary()) | binary(),
          caps=[] :: list()
         }).

-record(ref, {
          segment :: ecapnp:segment_id(),
          pos=-1 :: ecapnp:segment_pos(),
          offset=0 :: ecapnp:segment_offset(),
          kind=null :: ecapnp:ref_kind(),
          data :: #builder{} | #reader{}
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

-record(interface_ref, {
          pid :: pid()
         }).

-record(object, {
          ref=null :: ecapnp:ref(),
          schema :: atom() | ecapnp:schema_node()
         }).

-record(cap, {
          pid :: pid(),
          interface :: ecapnp:schema_node()
         }).



%% Capability & RPC

-record(request, {
          method :: ecapnp:field_name(),
          param :: ecapnp:object(),
          interface :: ecapnp:schema_node()
         }).
