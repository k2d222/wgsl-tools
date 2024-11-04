struct ElemStruct { a : u32 }
struct ElemStruct_ImplicitPadding { a : vec3<u32> }
struct ElemStruct_ExplicitPadding { @align(32) a : u32 }

      alias ArrayType = array<mat4x4<f32>>;
      struct Struct {
        
        arr : ArrayType,
      }
      @group(0) @binding(0) var<storage, read_write> buffer : Struct;
      @group(0) @binding(1) var<storage, read_write> length : u32;
      @compute @workgroup_size(1)
      fn main() {
        length = arrayLength(&buffer.arr);
      }
