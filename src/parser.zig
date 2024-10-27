const std = @import("std");
const token = @import("token.zig");
const chunk = @import("chunk.zig");
const core = @import("core.zig");
const scanner = @import("scanner.zig");
const object = @import("object.zig");

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
    LITERAL,
    STRING,
};

pub const Rule = struct {
    token.TokenType,
    OperationType,
    OperationType,
    Precedence,
};

pub const ParseRules: [46]Rule = [46]Rule{
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
    .{ token.TokenType.BANG, OperationType.UNARY, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.EQUAL, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.GREATER_THAN, OperationType.NONE, OperationType.BINARY, Precedence.COMPARISON },
    .{ token.TokenType.LESS_THAN, OperationType.NONE, OperationType.BINARY, Precedence.COMPARISON },
    .{ token.TokenType.CARET, OperationType.NONE, OperationType.BINARY, Precedence.FACTOR },
    .{ token.TokenType.PERCENT, OperationType.NONE, OperationType.BINARY, Precedence.FACTOR },
    .{ token.TokenType.AMPERSAND, OperationType.NONE, OperationType.BINARY, Precedence.FACTOR },
    .{ token.TokenType.COMMENT, OperationType.NONE, OperationType.NONE, Precedence.NONE }, // Useless

    // Multi-character tokens
    .{ token.TokenType.BANG_EQUAL, OperationType.NONE, OperationType.BINARY, Precedence.EQUALITY },
    .{ token.TokenType.DOUBLE_EQUAL, OperationType.NONE, OperationType.BINARY, Precedence.EQUALITY },
    .{ token.TokenType.GREATER_EQUAL_THAN, OperationType.NONE, OperationType.BINARY, Precedence.COMPARISON },
    .{ token.TokenType.LESS_EQUAL_THAN, OperationType.NONE, OperationType.BINARY, Precedence.COMPARISON },
    .{ token.TokenType.STAR_STAR, OperationType.NONE, OperationType.BINARY, Precedence.FACTOR },

    // Literals
    .{ token.TokenType.IDENTIFIER, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.STRING, OperationType.STRING, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.NUMBER, OperationType.NUMBER, OperationType.NONE, Precedence.NONE },

    // Keywords
    .{ token.TokenType.AND, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.OR, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.STRUCT, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.IF, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.ELSE, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.FALSE, OperationType.LITERAL, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.TRUE, OperationType.LITERAL, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.FOR, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.WHILE, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.FUNCTION, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.RETURN, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.SUPER, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.THIS, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.VAR, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.CONST, OperationType.NONE, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.NIL, OperationType.LITERAL, OperationType.NONE, Precedence.NONE },
    .{ token.TokenType.PRINT, OperationType.NONE, OperationType.NONE, Precedence.NONE },

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
        verbose: bool,

        /// Creates a new instance of the parser. This should be used only by the compiler.
        pub fn init(c: *chunk.Chunk(), source: []u8, verbose: bool) Self {
            const newScanner = scanner.Scanner().init(source);

            return Self{
                .current = null,
                .previous = null,
                .chunk = c,
                .source = source,
                .scanner = newScanner,
                .verbose = verbose,
            };
        }

        pub fn getScanner(self: *Self) *scanner.Scanner() {
            return &self.scanner;
        }

        /// Emits a pice of bytecode.
        pub fn emitByte(self: *Self, byte: u8) !void {
            const opCode = try core.OpCode.fromU8(byte);
            return self.chunk.writeOpCode(opCode, self.getScanner().line);
        }

        /// Emits an opcode of bytecode.
        pub fn emit(self: *Self, opCode: core.OpCode) !void {
            return self.chunk.writeOpCode(opCode, self.getScanner().line);
        }

        /// Emits the necessary bytecode to represent a constant.
        pub fn emitConstant(self: *Self, value: core.Value()) !void {
            try self.emit(core.OpCode.CONSTANT);
            try self.chunk.emitConstant(value);
        }

        /// Emits the necessary bytecode to represent the variable declaration.
        pub fn defineVariable(self: *Self, value: core.Value()) !void {
            try self.emit(core.OpCode.DEFG);
            try self.chunk.emitConstant(value);
        }

        /// Simple return function which emits the return opcode.
        pub fn end(self: *Self) !void {
            return self.emit(core.OpCode.RETURN);
        }

        fn number(self: *Self) !void {
            if (self.verbose) {
                std.debug.print("number() - {?}, {?}\n", .{ self.previous, self.current });
            }

            if (self.previous) |prev| {
                const numStr = try prev.toString(self.source);

                if (std.fmt.parseFloat(f64, numStr)) |num| {
                    try self.emitConstant(core.Value().initNumber(num));
                } else |err| {
                    std.debug.print("ParseFloatError: {s}\n", .{numStr});
                    return err;
                }
            }
        }

        fn unary(self: *Self) !void {
            if (self.verbose) {
                std.debug.print("unary() - {?}, {?}\n", .{ self.previous, self.current });
            }

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
                    .MINUS => return self.emit(core.OpCode.NEG),
                    .BANG => return self.emit(core.OpCode.NOT),
                    else => return,
                }
            }

            return core.CompilerError.UninitializedStack;
        }

        fn literal(self: *Self) !void {
            if (self.verbose) {
                std.debug.print("literal() {?}\n", .{self.previous});
            }

            if (self.previous) |prev| {
                return switch (prev.tokenType) {
                    .FALSE => self.emit(core.OpCode.FALSE),
                    .TRUE => self.emit(core.OpCode.TRUE),
                    .NIL => self.emit(core.OpCode.NIL),
                    else => core.CompilerError.InvalidOperation,
                };
            }
        }

        fn binary(self: *Self) !void {
            if (self.verbose) {
                std.debug.print("binary() - {?}, {?}\n", .{ self.previous, self.current });
            }

            if (self.previous) |previous| {
                const operatorType = previous.tokenType;
                const rule = try operatorType.getRule();

                const newPrec = @as(Precedence, @enumFromInt(@intFromEnum(rule[3]) + 1));
                try self.parsePrecedence(newPrec);

                return switch (operatorType) {
                    .PLUS => self.emit(core.OpCode.ADD),
                    .MINUS => self.emit(core.OpCode.SUB),
                    .STAR => self.emit(core.OpCode.MUL),
                    .SLASH => self.emit(core.OpCode.DIV),
                    .CARET => self.emit(core.OpCode.XOR),
                    .PERCENT => self.emit(core.OpCode.MOD),
                    .STAR_STAR => self.emit(core.OpCode.POW),
                    .AMPERSAND => self.emit(core.OpCode.AND),
                    .DOUBLE_EQUAL => self.emit(core.OpCode.EQ),
                    .BANG_EQUAL => self.emit(core.OpCode.NE),
                    .GREATER_THAN => self.emit(core.OpCode.GT),
                    .GREATER_EQUAL_THAN => self.emit(core.OpCode.GE),
                    .LESS_THAN => self.emit(core.OpCode.LT),
                    .LESS_EQUAL_THAN => self.emit(core.OpCode.LE),
                    else => core.CompilerError.InvalidOperation,
                };
            }

            return core.CompilerError.UninitializedStack;
        }

        pub fn expression(self: *Self) core.CompilerError!void {
            if (self.verbose) {
                std.debug.print("expression() - {?}, {?}\n", .{ self.previous, self.current });
            }

            return self.parsePrecedence(Precedence.ASSIGNMENT);
        }

        fn consumeVar(self: *Self) core.CompilerError!core.Value() {
            _ = self.consume(token.TokenType.IDENTIFIER) catch |err| {
                std.debug.print("ERROR: Expect variable name. {?}\n", .{err});
                return core.CompilerError.CompileError;
            };

            if (self.previous) |t| {
                const str = self.source[(t.pos)..(t.pos + t.size)];
                const strObj = object.String().init(str);

                if (object.Object().init(strObj)) |obj| {
                    const memory = std.heap.page_allocator;
                    const o = memory.create(object.Object()) catch |err| {
                        std.debug.print("ERROR: {?}\n", .{err});
                        return core.CompilerError.MemoryError;
                    };
                    o.* = obj;

                    return core.Value().initObj(o);
                }
            }

            return core.CompilerError.InvalidOperation;
        }

        fn varDeclaration(self: *Self) core.CompilerError!void {
            const globalVal = self.consumeVar() catch |err| {
                return err;
            };

            const isEqual = self.match(token.TokenType.EQUAL) catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.CompileError;
            };

            if (isEqual) {
                self.expression() catch |err| {
                    std.debug.print("ERROR: {?}\n", .{err});
                    return core.CompilerError.CompileError;
                };
            } else {
                self.emit(core.OpCode.NIL) catch |err| {
                    std.debug.print("ERROR: {?}\n", .{err});
                    return core.CompilerError.CompileError;
                };
            }

            _ = self.consume(token.TokenType.SEMICOLON) catch |err| {
                std.debug.print("Expected ';' after variable declaration. {?}\n", .{err});
                return core.CompilerError.CompileError;
            };

            self.defineVariable(globalVal) catch |err| {
                std.debug.print("ERROR: varDeclaration(): {?}\n", .{err});
                return core.CompilerError.CompileError;
            };
        }

        pub fn declaration(self: *Self) core.CompilerError!void {
            const isVar = self.match(token.TokenType.VAR) catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.CompileError;
            };

            if (isVar) {
                self.varDeclaration() catch |err| {
                    std.debug.print("ERROR: {?}\n", .{err});
                    return err;
                };
            } else {
                self.statement() catch |err| {
                    std.debug.print("ERROR: {?}\n", .{err});
                    return err;
                };
            }

            self.synchronize() catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.CompileError;
            };
        }

        fn synchronize(self: *Self) !void {
            var curr: token.Token() = undefined;

            if (self.current) |c| {
                curr = c;

                while (curr.tokenType != token.TokenType.EOF) {
                    if (self.previous) |prev| {
                        if (prev.tokenType == token.TokenType.SEMICOLON) {
                            return;
                        }
                    }

                    const shouldExit = switch (curr.tokenType) {
                        .STRUCT, .FUNCTION, .VAR, .CONST, .FOR, .IF, .WHILE, .PRINT, .RETURN => true,
                        else => false,
                    };

                    if (shouldExit) {
                        return;
                    }

                    _ = try self.advance();
                }
            }
        }

        fn expressionStatement(self: *Self) core.CompilerError!void {
            try self.expression();
            _ = self.consume(token.TokenType.SEMICOLON) catch |err| {
                std.debug.print("Expected ';' after expression. {?}\n", .{err});
                return core.CompilerError.CompileError;
            };

            self.emit(core.OpCode.POP) catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.CompileError;
            };
        }

        fn printStatement(self: *Self) core.CompilerError!void {
            _ = self.consume(token.TokenType.LEFT_PAREN) catch |err| {
                std.debug.print("Expected '(' after 'print'. {?}\n", .{err});
                return core.CompilerError.CompileError;
            };

            try self.expression();

            _ = self.consume(token.TokenType.RIGHT_PAREN) catch |err| {
                std.debug.print("Expected ')' after value or expression. {?}\n", .{err});
                return core.CompilerError.CompileError;
            };

            _ = self.consume(token.TokenType.SEMICOLON) catch |err| {
                std.debug.print("Expected ';' after statement. {?}\n", .{err});
                return core.CompilerError.CompileError;
            };

            self.emit(core.OpCode.OUT) catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.CompileError;
            };
        }

        fn statement(self: *Self) core.CompilerError!void {
            const isPrintStatement = self.match(token.TokenType.PRINT) catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.RuntimeError;
            };

            if (isPrintStatement) {
                try self.printStatement();
            } else {
                try self.expressionStatement();
            }
        }

        fn grouping(self: *Self) !void {
            if (self.verbose) {
                std.debug.print("grouping() - {?}, {?}\n", .{ self.previous, self.current });
            }

            try self.expression();

            if (self.consume(token.TokenType.RIGHT_PAREN)) |_| {
                return;
            } else |err| {
                return err;
            }
        }

        fn string(self: *Self) !void {
            if (self.previous) |prev| {
                const str = self.source[(prev.pos)..(prev.pos + prev.size)];
                const strObj = object.String().init(str);

                if (object.Object().init(strObj)) |obj| {
                    const memory = std.heap.page_allocator;
                    const o = try memory.create(object.Object());
                    o.* = obj;
                    // const o = @as(*object.Object(), @ptrCast(@constCast(&obj)));

                    self.emitConstant(core.Value().initObj(o)) catch |err| {
                        std.debug.print("ERROR: string(): {?}\n", .{err});
                        return err;
                    };

                    return;
                }
            }

            return core.CompilerError.UninitializedStack;
        }

        fn parsePrecedence(self: *Self, prec: Precedence) core.CompilerError!void {
            if (self.verbose) {
                std.debug.print("parsePrecedence() - {?}, {?}, {?}\n", .{ prec, self.previous, self.current });
            }

            // First we have to parse the prefix expression by reading the next token and
            // get the rule right after. This will give us the prefix operation.
            _ = self.advance() catch |err| {
                std.debug.print("ERROR: advance(): {}\n", .{err});
                return core.CompilerError.CompileError;
            };

            if (self.previous) |t| {
                const rule = t.tokenType.getRule() catch |err| {
                    std.debug.print("ERROR: token.getRule(): {}\n", .{
                        err,
                    });
                    return err;
                };
                const prefixRule: OperationType = rule[1];

                if (t.tokenType == token.TokenType.EOF) {
                    return;
                }

                // Return an error if there's no operation type available because we likely
                // hit some sort of syntax error.
                if (prefixRule == OperationType.NONE) {
                    return core.CompilerError.ExpectExpression;
                }

                if (prefixRule == OperationType.GROUPING) {
                    self.grouping() catch |err| {
                        std.debug.print("ERROR: grouping(): {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                } else if (prefixRule == OperationType.UNARY) {
                    self.unary() catch |err| {
                        std.debug.print("ERROR: unary(): {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                } else if (prefixRule == OperationType.NUMBER) {
                    self.number() catch |err| {
                        std.debug.print("ERROR: number(): {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                } else if (prefixRule == OperationType.LITERAL) {
                    self.literal() catch |err| {
                        std.debug.print("ERROR: literal(): {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                } else if (prefixRule == OperationType.STRING) {
                    self.string() catch |err| {
                        std.debug.print("ERROR: string(): {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                }

                if (self.current) |currentToken| {
                    if (currentToken.tokenType == token.TokenType.EOF) {
                        return;
                    }

                    var currentTokenRule = try currentToken.tokenType.getRule();

                    while (prec.toUsize() <= currentTokenRule[3].toUsize()) {
                        if (self.verbose) {
                            std.debug.print("prec <= currentTokenPrec {?}\n", .{currentToken.tokenType});
                        }

                        const newToken = self.advance() catch |err| {
                            std.debug.print("ERROR: {}\n", .{err});
                            return core.CompilerError.CompileError;
                        };

                        if (newToken.tokenType == token.TokenType.EOF) {
                            return;
                        }

                        const influxRule: OperationType = currentTokenRule[2];

                        if (influxRule == OperationType.BINARY) {
                            self.binary() catch |err| {
                                std.debug.print("ERROR: {}\n", .{err});
                                return core.CompilerError.CompileError;
                            };
                        }

                        if (self.current) |ct| {
                            currentTokenRule = try ct.tokenType.getRule();
                        }
                    }
                }
            }
        }

        pub fn advance(self: *Self) !token.Token() {
            if (self.verbose) {
                std.debug.print("advance() - {?}, {?}\n", .{ self.previous, self.current });
            }

            self.previous = self.current;

            while (true) {
                if (self.scanner.scanToken()) |t| {
                    self.current = t;
                    const tokenStr = try t.toString(self.source);

                    if (self.verbose) {
                        std.debug.print("\t{?} <= {s} adv\n", .{
                            t.tokenType,
                            tokenStr,
                        });
                    }

                    if (t.tokenType != token.TokenType.ERROR) {
                        return t;
                    }

                    return core.CompilerError.CompileError;
                } else |err| {
                    return err;
                }
            }

            return !self.current;
        }

        /// TODO: maybe add a message here?
        pub fn consume(self: *Self, tokenType: token.TokenType) !token.Token() {
            if (self.current) |current| {
                if (self.verbose) {
                    std.debug.print("> {} = {} - {}\n", .{ current.tokenType, tokenType, self.scanner.pos });
                }

                if (current.tokenType == tokenType) {
                    return self.advance();
                } else {
                    return core.CompilerError.UnexpectedToken;
                }
            }

            return core.CompilerError.UninitializedStack;
        }

        /// Check if the current token is of a specific type.
        pub fn check(self: *Self, tokenType: token.TokenType) !bool {
            if (self.verbose) {
                std.debug.print("check({?}) - {?}, {?}\n", .{ tokenType, self.previous, self.current });
            }

            if (self.current) |curr| {
                return curr.tokenType == tokenType;
            }

            return core.CompilerError.UninitializedStack;
        }

        /// Match the current token to the token type and then advance.
        pub fn match(self: *Self, tokenType: token.TokenType) !bool {
            if (self.verbose) {
                std.debug.print("match({?}) - {?}, {?}\n", .{ tokenType, self.previous, self.current });
            }

            if (!try self.check(tokenType)) {
                return false;
            }

            _ = try self.advance();

            return true;
        }
    };
}
