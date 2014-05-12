%% ecapnp schema records
%%
%% Bump version number on ANY change in ANY of the records in this file.
%% Must have the same version in both ecapnp runtime libs and compiled schemas.
-ecapnp_schema_version(3).

%% Common record for all schema nodes
-record(schema_node, {
          module :: atom(),
          name :: ecapnp:type_name(),
          id=0 :: ecapnp:type_id(),
          src = <<>> :: ecapnp:text(),
          kind=file :: ecapnp:schema_kind(),
          annotations=[] :: list(),
          nodes=[] :: ecapnp:schema_nodes(),
          scope=0 :: ecapnp:type_id()
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
          field
         }).

%% Annotation node
-record(annotation, {
          type,
          targets=[] :: list(atom())
         }).

%% Struct field
-record(field, {
          id,
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
          id,
          name,
          paramType,
          resultType
         }).
