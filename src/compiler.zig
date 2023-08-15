const std = @import("std");
const scanner = @import("scanner.zig");
const token = @import("token.zig");
const core = @import("core.zig");

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

        pub fn advance(self: *Self) !void {
            while (true) {
                if (self.scanner.scanToken()) |t| {
                    if (t.tokenType == token.TokenType.EOF) {
                        return;
                    } else if (t.tokenType == token.TokenType.ERROR) {
                        return core.CompilerError.CompileError;
                    }

                    var tokenStr = try t.toString(self.source);
                    std.debug.print("{?} <= {s}\n", .{
                        t.tokenType,
                        tokenStr,
                    });
                } else |err| {
                    std.debug.print("ERROR: line {d} / pos {d}\n", .{
                        self.scanner.line,
                        self.scanner.pos,
                    });
                    return err;
                }
            }
        }

        pub fn compile(self: *Self) !void {
            try self.advance();
        }
    };
}
