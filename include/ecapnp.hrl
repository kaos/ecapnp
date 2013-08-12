
-record(schema, { types = [] }).
-record(struct, { types = [], fields = [] }).
-record(enum, { values = [] }).
-record(ptr, { offset, type }).
-record(data, { type, offset, bits=0 }).
-record(object, { offset, type, schema, segment, segments }).
