const std = @import("std");
const vm = @import("vm.zig");
const chunk = @import("chunk.zig");
const core = @import("core.zig");

pub fn main() !void {
    const myVm = try vm.VM().init(false);
    var ck = myVm.chunk;
    // var c2 = chunk.Chunk().init(std.heap.page_allocator);
    try ck.writeOpCode(core.OpCode.CONSTANT, 1);
    try ck.addConstant(core.Value().initNumber(14.7391));
    try ck.writeOpCode(core.OpCode.CONSTANT, 2);
    try ck.addConstant(core.Value().initNumber(123.321));
    try ck.writeOpCode(core.OpCode.RETURN, 3);

    try ck.disassemble();
    try myVm.run();

    defer myVm.destroy();
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
