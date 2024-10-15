const std = @import("std");
const clap = @import("clap");
const vm = @import("vm.zig");
const chunk = @import("chunk.zig");
const core = @import("core.zig");
const compiler = @import("compiler.zig");
const utils = @import("utils.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    // https://github.com/Hejsil/zig-clap
    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display the help message.
        \\<str>...
        \\
    );

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = gpa.allocator(),
    }) catch |err| {
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        // const stdOut = std.io.getStdOut();
        // try clap.help(stdOut, clap.Help, &params, clap.HelpOptions{});{
        std.debug.print("--help\n", .{});
        std.process.exit(0);
    }

    var entry: []const u8 = "index.loom";

    if (res.positionals.len > 0) {
        entry = res.positionals[0];
    }

    var myVm = try vm.VM().init(false);

    if (utils.readFile(entry)) |source| {
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
