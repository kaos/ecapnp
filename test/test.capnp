# test/test.capnp
@0xe87e0317861d75a1;
struct Test @0xfa556038e27b336d {  # 16 bytes, 6 ptrs
  intField @0 :UInt8 = 33;  # bits[0, 8)
  textField @1 :Text = "test";  # ptr[0]
  structField @13 :Simple;  # ptr[4]
  union {  # tag bits [16, 32)
    boolField @2 :Bool;  # bits[8, 9), union tag = 0
    groupField :group {  # union tag = 1
      a @3 :Int8 = -44;  # bits[8, 16)
      b @4 :Int8 = 55;  # bits[32, 40)
      c @5 :Int8;  # bits[40, 48)
    }
  }
  opts :group {
    union {  # tag bits [64, 80)
      bool @6 :Bool = true;  # bits[48, 49), union tag = 0
      text @7 :Text;  # ptr[1], union tag = 1
      data @8 :Data;  # ptr[1], union tag = 2
      object @9 :Object;  # ptr[1], union tag = 3
    }
  }
  meta :group {
    id @10 :UInt16;  # bits[80, 96)
    tag @11 :Text;  # ptr[2]
    data @12 :Data = "1234";  # ptr[3]
    struct @14 :Simple = (message = "overriden default message", defaultValue = 321);  # ptr[5]
  }
}
struct Simple @0xd16f318851f71be8 {  # 8 bytes, 2 ptrs
  simpleMessage @2 :Text = "simple message";  # ptr[1]
  message @0 :Text = "default message";  # ptr[0]
  value @1 :UInt32 = 222;  # bits[0, 32)
  defaultValue @3 :UInt32 = 333;  # bits[32, 64)
}
