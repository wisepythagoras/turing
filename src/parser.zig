const std = @import("std");
const token = @import("token.zig");
const chunk = @import("chunk.zig");
const core = @import("core.zig");
const scanner = @import("scanner.zig");

pub const Precedence = enum(u8) {
    const Self = @This();

    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // > < >= <=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY,

    /// Gets the numerical representation of the enum value.
    pub fn toUsize(self: Self) usize {
        return @as(usize, @intFromEnum(self));
    }
};

pub const OperationType = enum(u8) {
    NONE,
    GROUPING,
    UNARY,
    BINARY,
    NUMBER,
};

pub const Rule = struct {
    token.TokenType,
    OperationType,
    OperationType,
    Precedence,
};

pub const ParseRules: [41]Rule = [41]Rule{
    .{ token.TokenType.LEFT_PAREN, OperationType.GROUPING, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.RIGHT_PAREN, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.LEFT_BRACE, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.RIGHT_BRACE, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.COMMA, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.DOT, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.MINUS, OperationType.UNARY, OperationType.BINARY, Precedence.TERM },
    .{ token.TokenType.PLUS, OperationType.NONE, OperationType.BINARY, Precedence.TERM },
    .{ token.TokenType.SEMICOLON, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.STAR, OperationType.NONE, OperationType.BINARY, Precedence.FACTOR },
    .{ token.TokenType.SLASH, OperationType.NONE, OperationType.BINARY, Precedence.FACTOR },
    .{ token.TokenType.BANG, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.EQUAL, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.GREATER_THAN, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.LESS_THAN, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.COMMENT, OperationType.NONE, OperationType.NONE, Precedence.NONE }, // Useless

    // Multi-character tokens
    .{ token.TokenType.BANG_EQUAL, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.DOUBLE_EQUAL, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.GREATER_EQUAL_THAN, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.LESS_EQUAL_THAN, OperationType.NONE, OperationType.NONE, Precedence.NONE },

    // Literals
    .{ token.TokenType.IDENTIFIER, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.STRING, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.NUMBER, OperationType.NUMBER, OperationType.NONE, Precedence.NONE },

    // Keywords
    .{ token.TokenType.AND, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.OR, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.STRUCT, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.IF, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.ELSE, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.FALSE, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.TRUE, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.FOR, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.WHILE, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.FUNCTION, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.RETURN, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.SUPER, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.THIS, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.VAR, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.CONST, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.NIL, OperationType.NONE, OperationType.NONE, Precedence.NONE },

    // Misc
    .{ token.TokenType.ERROR, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.EOF, OperationType.NONE, OperationType.NONE, Precedence.NONE },
};

pub fn Parser() type {
    return struct {
        const Self = @This();

        current: ?token.Token(),
        previous: ?token.Token(),
        chunk: *chunk.Chunk(),
        source: []u8,
        scanner: scanner.Scanner(),

        /// Creates a new instance of the parser. This should be used only by the compiler.
        pub fn init(c: *chunk.Chunk(), source: []u8) Self {
            const newScanner = scanner.Scanner().init(source);

            return Self{
                .current = null,
                .previous = null,
                .chunk = c,
                .source = source,
                .scanner = newScanner,
            };
        }

        pub fn getScanner(self: *Self) *scanner.Scanner() {
            return &self.scanner;
        }

        fn number(self: *Self) !void {
            if (self.previous) |prev| {
                try self.chunk.writeOpCode(core.OpCode.CONSTANT, 0);
                const numStr = try prev.toString(self.source);

                if (std.fmt.parseFloat(f64, numStr)) |num| {
                    try self.chunk.addConstant(core.Value().initNumber(num));
                } else |err| {
                    return err;
                }
            }
        }

        fn unary(self: *Self) !void {
            if (self.previous) |previous| {
                // The operator has been consumed and is stored in the previous token.
                const operatorType = previous.tokenType;

                // After parsing the operator, we parse the rest of the expression which we need to negate.
                // This could be a simple number (such as 1 => -1) or a more complex operation, which could
                // include a function call (ie: -(2 + myFn(a, b))).
                try self.parsePrecedence(Precedence.UNARY);

                // We should emit the opcode for the operation last, since we only want to push the number
                // onto the stack and then run the command on it.
                switch (operatorType) {
                    .BANG => return self.chunk.writeOpCode(core.OpCode.NEGATE, 0),
                    else => return,
                }
            }

            return core.CompilerError.UninitializedStack;
        }

        fn binary(self: *Self) !void {
            if (self.previous) |previous| {
                const operatorType = previous.tokenType;
                const rule = try operatorType.getRule();

                std.debug.print("{}\n", .{rule});

                const newPrec = @as(Precedence, @enumFromInt(@intFromEnum(rule[3]) + 1));
                try self.parsePrecedence(newPrec);

                return switch (operatorType) {
                    .PLUS => self.chunk.writeOpCode(core.OpCode.ADD, 0),
                    .MINUS => self.chunk.writeOpCode(core.OpCode.SUB, 0),
                    .STAR => self.chunk.writeOpCode(core.OpCode.MUL, 0),
                    .SLASH => self.chunk.writeOpCode(core.OpCode.DIV, 0),
                    else => core.CompilerError.InvalidOperation,
                };
            }

            return core.CompilerError.UninitializedStack;
        }

        pub fn expression(self: *Self) core.CompilerError!void {
            return self.parsePrecedence(Precedence.ASSIGNMENT);
        }

        fn empty(self: *Self) !void {
            _ = self;
        }

        fn grouping(self: *Self) !void {
            try self.expression();

            if (self.consume(token.TokenType.RIGHT_PAREN)) |_| {
                return;
            } else |err| {
                return err;
            }
        }

        fn parsePrecedence(self: *Self, prec: Precedence) core.CompilerError!void {
            const t = self.advance() catch |err| {
                std.debug.print("ERROR: advance(): {}\n", .{err});
                return core.CompilerError.CompileError;
            };
            const rule = t.tokenType.getRule() catch |err| {
                std.debug.print("ERROR: token.getRule(): {}\n", .{
                    err,
                });
                return err;
            };
            const prefixRule: OperationType = rule[1];

            if (prefixRule != OperationType.NONE) {
                std.debug.print("Run {}\n", .{prefixRule});
                if (prefixRule == OperationType.GROUPING) {
                    self.grouping() catch |err| {
                        std.debug.print("ERROR: {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                } else if (prefixRule == OperationType.UNARY) {
                    self.unary() catch |err| {
                        std.debug.print("ERROR: {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                } else if (prefixRule == OperationType.NUMBER) {
                    self.number() catch |err| {
                        std.debug.print("ERROR: {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                }
            }

            while (prec.toUsize() <= rule[3].toUsize()) {
                const t2 = self.advance() catch |err| {
                    std.debug.print("ERROR: {}\n", .{err});
                    return core.CompilerError.CompileError;
                };
                _ = t2;

                const influxRule: OperationType = rule[2];

                if (influxRule == OperationType.BINARY) {
                    self.binary() catch |err| {
                        std.debug.print("ERROR: {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                }
            }
        }

        pub fn advance(self: *Self) !token.Token() {
            self.previous = self.current;

            while (true) {
                if (self.scanner.scanToken()) |t| {
                    self.current = t;
                    const tokenStr = try t.toString(self.source);
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
                std.debug.print("> {} = {} - {}\n", .{ current.tokenType, tokenType, self.scanner.pos });
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
