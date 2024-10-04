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
            var newScanner = scanner.Scanner().init(source);
            const p = parser.Parser().init(c, source, &newScanner);
            return Self{
                .source = source,
                .scanner = newScanner,
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
            const tokens = std.ArrayList(token.Token()).init(std.heap.page_allocator);

            while (true) {
                if (self.scanner.scanToken()) |t| {
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
                        self.scanner.line,
                        self.scanner.pos,
                    });
                    return err;
                }
            }
        }

        /// Compiles and returns a chunk that's ready for the VM to run. To just dump every scanned
        /// token, run `scanAllTokens`.
        pub fn compile(self: *Self) !*chunk.Chunk() {
            if (self.parser.advance()) |t| {
                _ = t;
                return self.chunk;
            } else |err| {
                return err;
            }
        }
    };
}
