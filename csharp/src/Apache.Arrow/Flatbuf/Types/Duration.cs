// <auto-generated>
//  automatically generated by the FlatBuffers compiler, do not modify
// </auto-generated>

namespace Apache.Arrow.Flatbuf
{

using global::System;
using global::FlatBuffers;

internal struct Duration : IFlatbufferObject
{
  private Table __p;
  public ByteBuffer ByteBuffer { get { return __p.bb; } }
  public static Duration GetRootAsDuration(ByteBuffer _bb) { return GetRootAsDuration(_bb, new Duration()); }
  public static Duration GetRootAsDuration(ByteBuffer _bb, Duration obj) { return (obj.__assign(_bb.GetInt(_bb.Position) + _bb.Position, _bb)); }
  public void __init(int _i, ByteBuffer _bb) { __p.bb_pos = _i; __p.bb = _bb; }
  public Duration __assign(int _i, ByteBuffer _bb) { __init(_i, _bb); return this; }

  public TimeUnit Unit { get { int o = __p.__offset(4); return o != 0 ? (TimeUnit)__p.bb.GetShort(o + __p.bb_pos) : TimeUnit.MILLISECOND; } }

  public static Offset<Duration> CreateDuration(FlatBufferBuilder builder,
      TimeUnit unit = TimeUnit.MILLISECOND) {
    builder.StartObject(1);
    Duration.AddUnit(builder, unit);
    return Duration.EndDuration(builder);
  }

  public static void StartDuration(FlatBufferBuilder builder) { builder.StartObject(1); }
  public static void AddUnit(FlatBufferBuilder builder, TimeUnit unit) { builder.AddShort(0, (short)unit, 1); }
  public static Offset<Duration> EndDuration(FlatBufferBuilder builder) {
    int o = builder.EndObject();
    return new Offset<Duration>(o);
  }
};


}
