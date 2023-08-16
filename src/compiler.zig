const std = @import("std");
const scanner = @import("scanner.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const chunk = @import("chunk.zig");
const core = @import("core.zig");

pub fn Compiler() type {
    return struct {
        const Self = @This();

        source: []u8,
        scanner: scanner.Scanner(),
        chunk: *chunk.Chunk(),
        parser: parser.Parser(),

        pub fn init(source: []u8, c: *chunk.Chunk()) Self {
            var p = parser.Parser().init(c, source);
            return Self{
                .source = source,
                .scanner = scanner.Scanner().init(source),
                .chunk = c,
                .parser = p,
            };
        }

        pub fn interpret(self: *Self) !*chunk.Chunk() {
            return self.compile();
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

        pub fn compile(self: *Self) !*chunk.Chunk() {
            try self.advance();

            return self.chunk;
        }
    };
}
