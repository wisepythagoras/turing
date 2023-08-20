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

        fn number(self: *Self) !void {
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

        fn unary(self: *Self) !void {
            if (self.previous) |previous| {
                // The operator has been consumed and is stored in the previous token.
                var operatorType = previous.tokenType;

                // After parsing the operator, we parse the rest of the expression which we need to negate.
                // This could be a simple number (such as 1 => -1) or a more complex operation, which could
                // include a function call (ie: -(2 + myFn(a, b))).
                self.expression();

                switch (operatorType) {
                    .NEGATE => return self.chunk.writeOpCode(core.OpCode.NEGATE, 0),
                    else => return,
                }
            }

            return core.CompilerError.UninitializedStack;
        }

        fn expression(self: *Self) void {
            _ = self;
        }

        fn grouping(self: *Self) !void {
            self.expression();

            if (self.consume(token.TokenType.RIGHT_PAREN)) |_| {
                return;
            } else |err| {
                return err;
            }
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

        /// TODO: maybe add a message here?
        pub fn consume(self: *Self, tokenType: token.TokenType) !token.Token() {
            if (self.current) |current| {
                if (current.tokenType == tokenType) {
                    return self.advance();
                } else {
                    return core.CompilerError.UnexpectedToken;
                }
            }

            return core.CompilerError.UninitializedStack;
        }
    };
}
