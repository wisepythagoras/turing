const std = @import("std");
const vm = @import("vm.zig");
const chunk = @import("chunk.zig");
const core = @import("core.zig");
const compiler = @import("compiler.zig");
const utils = @import("utils.zig");

pub fn main() !void {
    var myVm = try vm.VM().init(false);

    if (utils.readFile("source.tur")) |source| {
        var comp = compiler.Compiler().init(source, myVm.chunk);

        // _ = try comp.scanAllTokens();

        // std.debug.print("----\n", .{});

        // To see every parsed token: scanAllTokens.
        if (comp.compile()) |_| {
            std.debug.print("Success\n", .{});
        } else |err| {
            std.debug.print("ERROR: compile(): {?}\n", .{err});
        }
    } else |err| {
        std.debug.print("{?}\n", .{err});
    }

    var ck = myVm.chunk;
    // // var c2 = chunk.Chunk().init(std.heap.page_allocator);
    // try ck.writeOpCode(core.OpCode.CONSTANT, 1);
    // try ck.addConstant(core.Value().initNumber(14.7391));
    // try ck.writeOpCode(core.OpCode.CONSTANT, 2);
    // try ck.addConstant(core.Value().initNumber(123.321));
    // try ck.writeOpCode(core.OpCode.NEGATE, 3);
    // try ck.writeOpCode(core.OpCode.ADD, 4);
    // try ck.writeOpCode(core.OpCode.CONSTANT, 5);
    // try ck.addConstant(core.Value().initNumber(-2));
    // try ck.writeOpCode(core.OpCode.MUL, 6);
    // try ck.writeOpCode(core.OpCode.RETURN, 7);

    try ck.disassemble();
    try myVm.run();

    defer myVm.destroy();
}

test "simple test" {
    const list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
