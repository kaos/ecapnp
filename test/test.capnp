# test/test.capnp
@0xe87e0317861d75a1;
struct Test @0xfa556038e27b336d {  # 16 bytes, 4 ptrs
  intField @0 :UInt8 = 33;  # bits[0, 8)
  textField @1 :Text = "test";  # ptr[0]
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
      bool @6 :Bool;  # bits[48, 49), union tag = 0
      text @7 :Text;  # ptr[1], union tag = 1
      data @8 :Data;  # ptr[1], union tag = 2
      object @9 :Object;  # ptr[1], union tag = 3
    }
  }
  meta :group {
    id @10 :UInt16;  # bits[80, 96)
    tag @11 :Text;  # ptr[2]
    data @12 :Data = "1234";  # ptr[3]
  }
}
