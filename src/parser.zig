const std = @import("std");
const token = @import("token.zig");
const chunk = @import("chunk.zig");
const core = @import("core.zig");

pub fn Parser() type {
    return struct {
        const Self = @This();

        // next: token.Token(),
        previous: ?token.Token(),
        chunk: *chunk.Chunk(),
        source: []u8,

        pub fn init(c: *chunk.Chunk(), source: []u8) Self {
            return Self{
                .previous = null,
                .chunk = c,
                .source = source,
            };
        }

        pub fn number(self: *Self) !void {
            if (self.previous) |prev| {
                self.chunk.writeOpCode(core.OpCode.CONSTANT, 0);
                var numStr = prev.toString(self.source);

                if (std.fmt.parseFloat(f64, numStr)) |num| {
                    self.chunk.addConstant(core.Value().initNumber(num));
                } else |err| {
                    return err;
                }
            }
        }
    };
}
