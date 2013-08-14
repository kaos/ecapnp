
-record(schema, { id, source, types=[] }).
-record(struct, { id, source, name, fields=[], types=[] }).
-record(enum, { values=[] }).
-record(ptr, { offset=0, type }).
-record(data, { type, offset=0, bits=0 }).
-record(object, { offset=0, type, schema, segment, segments, parent }).

