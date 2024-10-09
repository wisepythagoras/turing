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
        chunk: *chunk.Chunk(),
        parser: parser.Parser(),

        pub fn init(source: []u8, c: *chunk.Chunk()) Self {
            const p = parser.Parser().init(c, source);
            return Self{
                .source = source,
                .chunk = c,
                .parser = p,
            };
        }

        pub fn interpret(self: *Self) !*chunk.Chunk() {
            return self.compile();
        }

        /// This function scans all tokens beforehand and then makes it possible to feed them into
        /// the parser so it runs them all at once. The problem with this approach is that it's going
        /// to be slower.
        pub fn scanAllTokens(self: *Self) !std.ArrayList(token.Token()) {
            var tokens = std.ArrayList(token.Token()).init(std.heap.page_allocator);

            while (true) {
                if (self.parser.getScanner().scanToken()) |t| {
                    try tokens.append(t);

                    if (t.tokenType == token.TokenType.EOF) {
                        return tokens;
                    } else if (t.tokenType == token.TokenType.ERROR) {
                        return core.CompilerError.CompileError;
                    }

                    const tokenStr = try t.toString(self.source);
                    std.debug.print("{?} <= {s}\n", .{
                        t.tokenType,
                        tokenStr,
                    });
                } else |err| {
                    std.debug.print("ERROR: line {d} / pos {d}\n", .{
                        self.parser.getScanner().line,
                        self.parser.getScanner().pos,
                    });
                    return err;
                }
            }
        }

        pub fn end(self: *Self) !void {
            return self.chunk.writeOpCode(core.OpCode.RETURN, self.parser.getScanner().line);
        }

        /// Compiles and returns a chunk that's ready for the VM to run. To just dump every scanned
        /// token, run `scanAllTokens`.
        pub fn compile(self: *Self) !*chunk.Chunk() {
            if (self.parser.advance()) |t| {
                _ = t;

                try self.parser.expression();
                if (self.parser.consume(token.TokenType.EOF)) |_| {
                    return self.chunk;
                } else |err| {
                    return err;
                }
            } else |err| {
                return err;
            }
        }
    };
}
