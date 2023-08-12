const std = @import("std");
const scanner = @import("scanner.zig");
const token = @import("token.zig");

pub fn Compiler() type {
    return struct {
        const Self = @This();

        source: []u8,
        scanner: scanner.Scanner(),

        pub fn init(source: []u8) Self {
            return Self{
                .source = source,
                .scanner = scanner.Scanner().init(source),
            };
        }

        pub fn interpret(self: *Self) !void {
            try self.compile();
        }

        pub fn compile(self: *Self) !void {
            while (true) {
                var t = self.scanner.scanToken();

                if (t.tokenType == token.TokenType.EOF) {
                    return;
                }

                var tokenStr = try t.toString(self.source);
                std.debug.print("{?} <= {s}\n", .{
                    t.tokenType,
                    tokenStr,
                });
            }
        }
    };
}
