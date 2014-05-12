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
          offset=0 :: ecapnp:segment_offset(), %% or capability index in CapTable for #interface_ref{}'s
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
          ref=null :: #builder{} | #reader{}, %% | #promise{} | #rpc_call{},
          schema :: atom() | ecapnp:schema_node()
         }).


%% Capability & RPC

-record(capability, {
          id :: {local, pid()} | {remote, {non_neg_integer(), pid()}},
          interfaces=[] :: list(ecapnp:schema_node())
         }).

-record(promise, {
          vat :: pid(),
          ref :: reference(),
          resultType :: ecapnp:type_id(),
          transform=[] :: list()
         }).

-record(rpc_call, {
          target :: #capability{} | #promise{},
          interface :: ecapnp:schema_node(),
          method :: #method{},
          params :: ecapnp:object()
         }).
