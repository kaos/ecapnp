
-record(schema, { id, source, types=[] }).
-record(struct, { id, source, name, dsize, psize, fields=[], types=[] }).
-record(enum, { values=[] }).
-record(ptr, { offset=0, idx=0, type }).
-record(data, { type, offset=0, bits=0, align=0 }).
-record(object, { offset=0, type, schema, segment, segments, parent }).

