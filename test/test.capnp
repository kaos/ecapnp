# test.capnp
@0xe87e0317861d75a1;
$testAnno("file anno 2013");
annotation testAnno @0xf0fabdffa4323aca (*) :Text;
struct Test @0xfa556038e27b336d $testAnno("Test struct anno 2013 too") {  # 16 bytes, 6 ptrs
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
      object @9 :AnyPointer;  # ptr[1], union tag = 3
    }
  }
  meta :group {
    id @10 :UInt16;  # bits[80, 96)
    tag @11 :Text;  # ptr[2]
    data @12 :Data = "1234";  # ptr[3]
    struct @14 :Simple = (message = "overriden default message", value = 222, defaultValue = 321);  # ptr[5]
  }
}
struct Simple @0xd16f318851f71be8 {  # 8 bytes, 2 ptrs
  simpleMessage @2 :Text = "simple message";  # ptr[1]
  message @0 :Text = "default message";  # ptr[0]
  value @1 :UInt32 = 222;  # bits[0, 32)
  defaultValue @3 :UInt32 = 333;  # bits[32, 64)
}
struct ListTest @0xed15f6a91b7977a6 {  # 0 bytes, 4 ptrs
  listInts @0 :List(Int32) = [456, 789, -123];  # ptr[0]
  listAny @1 :AnyPointer;  # ptr[1]
  listSimples @2 :List(Simple) = [(message = "first", value = 1, defaultValue = 333), (message = "second", value = 2, defaultValue = 333)];  # ptr[2]
  listText @3 :List(Text);  # ptr[3]
}
interface BasicCap @0xf329462caa09f38f {
  add @0 (a :Int64, b :Int64) -> (result :Int64);
}
interface Pipelines @0xde7af08d2279ac69 {
  getBasic @0 () -> (basic :BasicCap);
}
interface OtherCap @0x9000899726987e7f {
  sqroot @0 (a :Int64) -> (root1 :Float64, root2 :Float64);
}
interface ThirdCap @0xfb84e23a9ca71e0e extends(BasicCap, OtherCap) {
  square @0 (a :Int64) -> (sq :Int64);
}
struct CapTest @0xf21107d21522cfc8 {  # 0 bytes, 2 ptrs
  basic @0 :BasicCap;  # ptr[0]
  obj @1 :AnyPointer;  # ptr[1]
}
