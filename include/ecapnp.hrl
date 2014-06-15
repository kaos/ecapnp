-ifndef(ecapnp_hrl).
-define(ecapnp_hrl,1).

-include("ecapnp_records.hrl").

-type annotation() :: ecapnp:annotation().
-type bit_count() :: ecapnp:bit_count().
-type const() :: ecapnp:const().
-type data() :: ecapnp:data().
-type element_size() :: ecapnp:element_size().
-type enum() :: ecapnp:enum().
-type enum_values() :: ecapnp:enum_values().
-type far_ref() :: ecapnp:far_ref().
-type field_name() :: ecapnp:field_name().
-type field_type() :: ecapnp:field_type().
-type field_value() :: ecapnp:field_value().
-type group() :: ecapnp:group().
-type interface() :: ecapnp:interface().
-type list_ref() :: ecapnp:list_ref().
-type message() :: ecapnp:message().
-type msg() :: ecapnp:msg().
-type node_type() :: ecapnp:node_type().
-type node_types() :: ecapnp:node_types().
-type object() :: ecapnp:object().
-type object_field() :: ecapnp:object_field().
-type object_fields() :: ecapnp:object_fields().
-type ptr() :: ecapnp:ptr().
-type ptr_count() :: ecapnp:ptr_count().
-type ptr_index() :: ecapnp:ptr_index().
-type ref() :: ecapnp:ref().
-type ref_kind() :: ecapnp:ref_kind().
-type schema() :: ecapnp:schema().
-type schema_kind() :: ecapnp:schema_kind().
-type schema_node() :: ecapnp:schema_node().
-type schema_nodes() :: ecapnp:schema_nodes().
-type schema_type() :: ecapnp:schema_type().
-type segment_id() :: ecapnp:segment_id().
-type segment_offset() :: ecapnp:segment_offset().
-type segment_pos() :: ecapnp:segment_pos().
-type struct() :: ecapnp:struct().
-type struct_fields() :: ecapnp:struct_fields().
-type struct_ref() :: ecapnp:struct_ref().
-type text() :: ecapnp:text().
-type type_id() :: ecapnp:type_id().
-type type_name() :: ecapnp:type_name().
-type value() :: ecapnp:value().
-type value_type() :: ecapnp:value_type().
-type word_count() :: ecapnp:word_count().


-ifdef(ECAPNP_TRACE).

-ifndef(ECAPNP_GEN_OPTS).
-define(ECAPNP_GEN_OPTS,[{debug, [trace]}]).
-endif.

-define(ECAPNP_DEBUG,1).

-else.

-ifndef(ECAPNP_GEN_OPTS).
-define(ECAPNP_GEN_OPTS,[]).
-endif.

-endif.

-ifndef(ECAPNP_DEBUG).
-define(DBG(F,A),ok).
-define(DUMP(D),).

-else.
-define(DBG(F,A),
        _ = io:format(%%standard_error,
                      "~-10w ~20s:~-4w\t~s~n",
                      [self(), ?MODULE, ?LINE, io_lib:format(F, A)])
       ).
-define(DUMP(D), ecapnp:dump(D)).

-endif.

-endif.
