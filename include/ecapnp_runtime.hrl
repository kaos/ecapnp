-record(builder, {
          pid :: pid()
         }).

-record(reader, {
          data :: list(binary()) | binary(),
          caps = [] :: list()
         }).

-record(ref, {
          segment :: ecapnp:segment_id(),
          pos = -1 :: ecapnp:segment_pos(),
          offset = 0 :: ecapnp:segment_offset(), %% or capability index in CapTable for #interface_ref{}'s
          align = 0 :: ecapnp:bit_count(),
          kind = null :: ecapnp:ref_kind(),
          data :: #builder{} | #reader{}
         }).

-record(struct_ref, {
          dsize = 0 :: ecapnp:word_count(),
          psize = 0 :: ecapnp:ptr_count()
         }).

-record(list_ref, {
          size = 0 :: ecapnp:bit_count() | pointer | {inlineComposite, #struct_ref{}},
          count = 0 :: non_neg_integer() %% ALWAYS number of elements in list
         }).

-record(far_ref, {
          segment = 0 :: non_neg_integer(),
          double_far = false :: boolean()
         }).

-record(interface_ref, {
          owner :: {atom(), pid()},
          id :: term()
         }).

-record(object, {
          ref = null :: #ref{},
          schema :: atom() | ecapnp:schema_node()
         }).

-record(rpc_call, {
          target :: term(),
          interface :: ecapnp:type_id(),
          method :: non_neg_integer(),
          params :: ecapnp:object(),
          results :: ecapnp:object(),
          resultSchema = object :: ecapnp:schema_node() | object
         }).

-record(promise, {
          owner :: {atom(), pid()},
          pid :: pid(), %% ecapnp_promise pid
          transform = [] :: list(),
          schema :: ecapnp:schema_node()
         }).
