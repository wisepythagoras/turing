const std = @import("std");
const token = @import("token.zig");
const chunk = @import("chunk.zig");
const core = @import("core.zig");

pub fn Parser() type {
    return struct {
        const Self = @This();

        current: ?token.Token(),
        previous: ?token.Token(),
        chunk: *chunk.Chunk(),
        source: []u8,

        /// Creates a new instance of the parser. This should be used only by the compiler.
        pub fn init(c: *chunk.Chunk(), source: []u8) Self {
            return Self{
                .current = null,
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

        fn grouping(self: *Self) void {
            _ = self;
        }
    };
}
