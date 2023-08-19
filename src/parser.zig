const std = @import("std");
const token = @import("token.zig");
const chunk = @import("chunk.zig");
const core = @import("core.zig");
const scanner = @import("scanner.zig");

pub fn Parser() type {
    return struct {
        const Self = @This();

        current: ?token.Token(),
        previous: ?token.Token(),
        chunk: *chunk.Chunk(),
        source: []u8,
        scanner: *scanner.Scanner(),

        /// Creates a new instance of the parser. This should be used only by the compiler.
        pub fn init(c: *chunk.Chunk(), source: []u8, sPtr: *scanner.Scanner()) Self {
            return Self{
                .current = null,
                .previous = null,
                .chunk = c,
                .source = source,
                .scanner = sPtr,
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

        pub fn advance(self: *Self) !token.Token() {
            self.previous = self.current;

            while (true) {
                if (self.scanner.scanToken()) |t| {
                    self.current = t;
                    var tokenStr = try t.toString(self.source);
                    std.debug.print("{?} <= {s}\n", .{
                        t.tokenType,
                        tokenStr,
                    });
                    return t;
                } else |err| {
                    return err;
                }
            }
        }
    };
}
