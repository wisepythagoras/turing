const std = @import("std");
const scanner = @import("scanner.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const chunk = @import("chunk.zig");
const core = @import("core.zig");
const opcode = @import("opcode.zig");
const local = @import("local.zig");

pub fn Compiler() type {
    return struct {
        const Self = @This();

        source: []u8,
        chunk: *chunk.Chunk(),
        parser: parser.Parser(),
        locals: std.ArrayList(local.Local()),
        localCount: usize,
        scopeDepth: usize,
        verbose: bool,

        pub fn init(source: []u8, c: *chunk.Chunk(), verbose: bool) !Self {
            const memory = std.heap.c_allocator;
            const p = parser.Parser().init(c, source, verbose);
            const locals = std.ArrayList(local.Local()).init(memory);

            return Self{
                .source = source,
                .chunk = c,
                .parser = p,
                .locals = locals,
                .localCount = 0,
                .scopeDepth = 0,
                .verbose = verbose,
            };
        }

        pub fn interpret(self: *Self) !*chunk.Chunk() {
            return self.compile();
        }

        /// This function scans all tokens beforehand and then makes it possible to feed them into
        /// the parser so it runs them all at once. The problem with this approach is that it's going
        /// to be slower.
        pub fn scanAllTokens(self: *Self) !std.ArrayList(token.Token()) {
            var tokens = std.ArrayList(token.Token()).init(std.heap.c_allocator);

            while (true) {
                if (self.parser.getScanner().scanToken()) |t| {
                    try tokens.append(t);

                    if (t.tokenType == token.TokenType.EOF) {
                        return tokens;
                    } else if (t.tokenType == token.TokenType.ERROR) {
                        return core.CompilerError.CompileError;
                    }

                    const tokenStr = try t.toString(self.source);

                    if (self.verbose) {
                        std.debug.print("{?} <= {s}\n", .{
                            t.tokenType,
                            tokenStr,
                        });
                    }
                } else |err| {
                    std.debug.print("ERROR: line {d} / pos {d}\n", .{
                        self.parser.getScanner().line,
                        self.parser.getScanner().pos,
                    });
                    return err;
                }
            }
        }

        /// Emits a pice of bytecode.
        pub fn emitByte(self: *Self, byte: u8) !void {
            const opCode = try opcode.OpCode.fromU8(byte);
            return self.chunk.writeOpCode(opCode, self.parser.getScanner().line);
        }

        /// Emits an opcode of bytecode.
        pub fn emit(self: *Self, opCode: opcode.OpCode) !void {
            return self.chunk.writeOpCode(opCode, self.parser.getScanner().line);
        }

        /// Emits the necessary bytecode to represent a constant.
        pub fn emitConstant(self: *Self, value: core.Value()) !void {
            try self.emit(opcode.OpCode.CONSTANT);
            try self.chunk.emitConstant(value);
        }

        /// Simple return function which emits the return opcode.
        pub fn end(self: *Self) !void {
            return self.emit(opcode.OpCode.RETURN);
        }

        /// Increase the scope depth.
        pub fn beginScope(self: *Self) void {
            self.scopeDepth += 1;
        }

        /// Decrease the scope depth.
        pub fn endScope(self: *Self) !void {
            self.scopeDepth -= 1;

            while (self.localCount > 0 and self.locals.getLast().depth > self.scopeDepth) {
                try self.emit(opcode.OpCode.POP);
                _ = self.locals.pop();
                self.localCount -= 1;
            }
        }

        /// Resolve a local variable by its name.
        pub fn resolveLocalVar(self: *Self, name: *token.Token()) ?local.LocalTuple {
            for (0..self.localCount) |index| {
                const i = (self.localCount - 1) - index;
                const lv = self.locals.items[i];

                if (name.equals(lv.name, self.source) and lv.depth > 0) {
                    const memory = std.heap.c_allocator;
                    const lvPtr = memory.create(local.Local()) catch |err| {
                        std.debug.print("ERROR: {?} (create ptr)\n", .{err});
                        return null;
                    };

                    lvPtr.* = lv;

                    return .{ lvPtr, i };
                }
            }

            return null;
        }

        /// We mark the last variable added to the `locals` array as initialized by setting the
        /// `depth` field to the `scopeDepth`.
        pub fn markLastLocalVarInitialized(self: *Self) !void {
            if (self.localCount == 0) {
                return core.CompilerError.UninitializedStack;
            }

            self.locals.items[self.localCount - 1].depth = self.scopeDepth;
        }

        /// Compiles and returns a chunk that's ready for the VM to run. To just dump every scanned
        /// token, run `scanAllTokens`.
        pub fn compile(self: *Self) !*chunk.Chunk() {
            if (self.verbose) {
                std.debug.print("compile()\n", .{});
            }

            if (self.parser.advance()) |t| {
                _ = t;

                while (!try self.parser.match(token.TokenType.EOF)) {
                    try self.parser.declaration();
                }
            } else |err| {
                return err;
            }

            return self.chunk;
        }
    };
}
