const std = @import("std");
const chunk = @import("chunk.zig");
const core = @import("core.zig");

pub fn main() !void {
    var c2 = chunk.Chunk().init(std.heap.page_allocator);
    try c2.writeOpCode(core.OpCode.CONSTANT, 1);
    try c2.addConstant(14.7391);
    try c2.writeOpCode(core.OpCode.RETURN, 2);

    c2.disassemble();

    defer c2.destroy();
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
