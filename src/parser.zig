const std = @import("std");
const token = @import("token.zig");
const chunk = @import("chunk.zig");
const core = @import("core.zig");
const scanner = @import("scanner.zig");
const object = @import("object.zig");
const opcode = @import("opcode.zig");
const compiler = @import("compiler.zig");
const local = @import("local.zig");
const utils = @import("utils.zig");

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
    VARIABLE,
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
    .{ token.TokenType.IDENTIFIER, OperationType.VARIABLE, OperationType.NONE, Precedence.NONE },
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
        compiler: *compiler.Compiler(),
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
                .compiler = undefined,
                .verbose = verbose,
            };
        }

        pub fn setCompiler(self: *Self, comp: *compiler.Compiler()) void {
            self.compiler = comp;
        }

        pub fn getScanner(self: *Self) *scanner.Scanner() {
            return &self.scanner;
        }

        /// Emits a pice of bytecode.
        pub fn emitByte(self: *Self, byte: u8) !void {
            const opCode = try opcode.OpCode.fromU8(byte);
            return self.chunk.writeOpCode(opCode, self.getScanner().line);
        }

        /// Emits an opcode of bytecode.
        pub fn emit(self: *Self, opCode: opcode.OpCode) !void {
            return self.chunk.writeOpCode(opCode, self.getScanner().line);
        }

        /// Emits the necessary bytecode to represent a constant.
        pub fn emitConstant(self: *Self, value: core.Value()) !void {
            try self.emit(opcode.OpCode.CONSTANT);
            try self.chunk.emitConstant(value);
        }

        /// Emits the necessary bytecode to represent the variable declaration.
        pub fn defineVariable(self: *Self, value: core.Value()) !void {
            try self.emit(opcode.OpCode.DEFG);
            try self.chunk.emitConstant(value);
        }

        /// Simple return function which emits the return opcode.
        pub fn end(self: *Self) !void {
            return self.emit(opcode.OpCode.RETURN);
        }

        fn number(self: *Self, canAssign: bool) !void {
            _ = canAssign;

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

        fn unary(self: *Self, canAssign: bool) !void {
            _ = canAssign;

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
                    .MINUS => return self.emit(opcode.OpCode.NEG),
                    .BANG => return self.emit(opcode.OpCode.NOT),
                    else => return,
                }
            }

            return core.CompilerError.UninitializedStack;
        }

        fn literal(self: *Self, canAssign: bool) !void {
            _ = canAssign;

            if (self.verbose) {
                std.debug.print("literal() {?}\n", .{self.previous});
            }

            if (self.previous) |prev| {
                return switch (prev.tokenType) {
                    .FALSE => self.emit(opcode.OpCode.FALSE),
                    .TRUE => self.emit(opcode.OpCode.TRUE),
                    .NIL => self.emit(opcode.OpCode.NIL),
                    else => core.CompilerError.InvalidOperation,
                };
            }
        }

        fn binary(self: *Self, canAssign: bool) !void {
            _ = canAssign;

            if (self.verbose) {
                std.debug.print("binary() - {?}, {?}\n", .{ self.previous, self.current });
            }

            if (self.previous) |previous| {
                const operatorType = previous.tokenType;
                const rule = try operatorType.getRule();

                const newPrec = @as(Precedence, @enumFromInt(@intFromEnum(rule[3]) + 1));
                try self.parsePrecedence(newPrec);

                return switch (operatorType) {
                    .PLUS => self.emit(opcode.OpCode.ADD),
                    .MINUS => self.emit(opcode.OpCode.SUB),
                    .STAR => self.emit(opcode.OpCode.MUL),
                    .SLASH => self.emit(opcode.OpCode.DIV),
                    .CARET => self.emit(opcode.OpCode.XOR),
                    .PERCENT => self.emit(opcode.OpCode.MOD),
                    .STAR_STAR => self.emit(opcode.OpCode.POW),
                    .AMPERSAND => self.emit(opcode.OpCode.AND),
                    .DOUBLE_EQUAL => self.emit(opcode.OpCode.EQ),
                    .BANG_EQUAL => self.emit(opcode.OpCode.NE),
                    .GREATER_THAN => self.emit(opcode.OpCode.GT),
                    .GREATER_EQUAL_THAN => self.emit(opcode.OpCode.GE),
                    .LESS_THAN => self.emit(opcode.OpCode.LT),
                    .LESS_EQUAL_THAN => self.emit(opcode.OpCode.LE),
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

        fn declareVariable(self: *Self, name: *token.Token()) !void {
            const localVar = local.Local().new(name, 0);

            for (0..self.compiler.localCount) |index| {
                // TODO: Maybe there's a logic issue here, but I'm too tired to understand it.
                const i = (self.compiler.localCount - 1) - index;
                const l = self.compiler.locals.items[i];

                if (l.depth != -1 and l.depth < self.compiler.scopeDepth) {
                    break;
                }

                // This code should return an error only if we encounter the redeclaration of a variable
                // in the local scope.
                if (l.name.equals(name, self.source)) {
                    std.debug.print("ERROR: Redeclaration of local variable \"{s}\"\n", .{name.toStringSimple(self.source)});
                    return core.CompilerError.Redeclaration;
                }
            }

            self.compiler.locals.append(localVar) catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.CompileError;
            };
            self.compiler.localCount += 1;
        }

        fn parseVariable(self: *Self) core.CompilerError!core.Value() {
            _ = self.consume(token.TokenType.IDENTIFIER) catch |err| {
                std.debug.print("ERROR: Expect variable name. {?}\n", .{err});
                return core.CompilerError.CompileError;
            };

            if (self.previous) |t| {
                const str = self.source[(t.pos)..(t.pos + t.size)];

                if (self.compiler.scopeDepth > 0) {
                    const memory = std.heap.page_allocator;
                    const tokenPtr = memory.create(token.Token()) catch |err| {
                        std.debug.print("ERROR: {?}\n", .{err});
                        return core.CompilerError.MemoryError;
                    };

                    tokenPtr.* = t;
                    try self.declareVariable(tokenPtr);

                    // For local variables we don't care to store the variable name. So we just need
                    // to return a dummy value here, which is nil in our case, since we don't care
                    // about it.
                    return core.Value().initNil();
                }

                return utils.strToObject(str) catch |err| {
                    std.debug.print("ERROR: {?}\n", .{err});
                    return core.CompilerError.MemoryError;
                };
            }

            return core.CompilerError.InvalidOperation;
        }

        /// A variable declaration looks something like this: `let myVar = expression;`.
        /// TODO: Handle constants as well.
        fn varDeclaration(self: *Self) core.CompilerError!void {
            const v = self.parseVariable() catch |err| {
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
                self.emit(opcode.OpCode.NIL) catch |err| {
                    std.debug.print("ERROR: {?}\n", .{err});
                    return core.CompilerError.CompileError;
                };
            }

            _ = self.consume(token.TokenType.SEMICOLON) catch |err| {
                std.debug.print("Expected ';' after variable declaration. {?}\n", .{err});
                return core.CompilerError.CompileError;
            };

            // The code below should only be run for global variables. We create them out of the
            // current state of the VM. The VM will execute the code for the variable's creation
            // and the value will be on the stack. That's also where we have our locals allocated.
            // So basically we don't need to do anything else. The temporary value on the stack
            // becomes the local variable.
            if (self.compiler.scopeDepth == 0) {
                self.defineVariable(v) catch |err| {
                    std.debug.print("ERROR: varDeclaration(): {?}\n", .{err});
                    return core.CompilerError.CompileError;
                };
            } else {
                try self.compiler.markLastLocalVarInitialized();
            }
        }

        /// Handle declarations of any kind, such as variables or just statements.
        pub fn declaration(self: *Self) core.CompilerError!void {
            const isVar = self.match(token.TokenType.VAR) catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.CompileError;
            };

            if (isVar) {
                self.varDeclaration() catch |err| {
                    std.debug.print("ERROR: {?}\n", .{err});

                    self.synchronize() catch |syncErr| {
                        std.debug.print("ERROR: {?}\n", .{syncErr});
                        return core.CompilerError.CompileError;
                    };

                    return err;
                };
            } else {
                self.statement() catch |err| {
                    std.debug.print("ERROR: {?}\n", .{err});

                    self.synchronize() catch |syncErr| {
                        std.debug.print("ERROR: {?}\n", .{syncErr});
                        return core.CompilerError.CompileError;
                    };

                    return err;
                };
            }
        }

        fn synchronize(self: *Self) !void {
            if (self.verbose) {
                std.debug.print("synchronize()\n", .{});
            }

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

                    std.debug.print("-> {any}\n", .{curr});

                    curr = try self.advance();
                }
            }
        }

        fn expressionStatement(self: *Self) core.CompilerError!void {
            try self.expression();
            _ = self.consume(token.TokenType.SEMICOLON) catch |err| {
                std.debug.print("Expected ';' after expression. {?}\n", .{err});
                return core.CompilerError.CompileError;
            };

            self.emit(opcode.OpCode.POP) catch |err| {
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

            self.emit(opcode.OpCode.OUT) catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.CompileError;
            };
        }

        fn block(self: *Self) core.CompilerError!void {
            while (!try self.check(token.TokenType.RIGHT_BRACE) and !try self.check(token.TokenType.EOF)) {
                try self.declaration();
            }

            _ = self.consume(token.TokenType.RIGHT_BRACE) catch |err| {
                std.debug.print("ERROR: Expected '{c}' to close the block. {?}\n", .{ '}', err });
                return err;
            };
        }

        fn statement(self: *Self) core.CompilerError!void {
            const isPrintStatement = self.match(token.TokenType.PRINT) catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.RuntimeError;
            };
            const isLeftBrace = self.match(token.TokenType.LEFT_BRACE) catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.RuntimeError;
            };

            if (isPrintStatement) {
                try self.printStatement();
            } else if (isLeftBrace) {
                self.compiler.beginScope();
                try self.block();
                self.compiler.endScope() catch |err| {
                    std.debug.print("ERROR: {?}\n", .{err});
                    return core.CompilerError.CompileError;
                };
            } else {
                try self.expressionStatement();
            }
        }

        fn grouping(self: *Self, canAssign: bool) !void {
            _ = canAssign;

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

        fn string(self: *Self, canAssign: bool) !void {
            _ = canAssign;

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

        fn namedVariable(self: *Self, t: token.Token(), canAssign: bool) !void {
            var getOp: opcode.OpCode = undefined;
            var setOp: opcode.OpCode = undefined;
            var value: core.Value() = undefined;

            const memory = std.heap.page_allocator;
            const tokenPtr = memory.create(token.Token()) catch |err| {
                std.debug.print("ERROR: {?}\n", .{err});
                return core.CompilerError.MemoryError;
            };
            tokenPtr.* = t;

            if (self.compiler.resolveLocalVar(tokenPtr)) |idx| {
                getOp = opcode.OpCode.GETL;
                setOp = opcode.OpCode.SETL;

                value = core.Value().initNumber(@as(f64, @floatFromInt(idx)));
                memory.destroy(tokenPtr);
            } else {
                getOp = opcode.OpCode.GETG;
                setOp = opcode.OpCode.SETG;

                const str = self.source[(t.pos)..(t.pos + t.size)];
                value = utils.strToObject(str) catch |err| {
                    std.debug.print("ERROR: {?}\n", .{err});
                    return core.CompilerError.MemoryError;
                };
            }

            if (self.match(token.TokenType.EQUAL)) |isEqual| {
                if (canAssign and isEqual) {
                    try self.expression();

                    try self.emit(setOp);
                    try self.chunk.emitConstant(value);
                } else {
                    try self.emit(getOp);
                    try self.chunk.emitConstant(value);
                }
            } else |err| {
                return err;
            }
        }

        /// Handle the variable declaration.
        fn variable(self: *Self, canAssign: bool) !void {
            if (self.previous) |prev| {
                try self.namedVariable(prev, canAssign);
            }
        }

        /// Parse an expression taking precedence in consideration. So, for example, `1 + 2 * 3` will
        /// evaluate `2 * 3` before `1 + 6`. And it works in a similar way with grouping (expressions
        /// within a parenthesis).
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

                const canAssign = prec.toUsize() <= Precedence.ASSIGNMENT.toUsize();

                if (prefixRule == OperationType.GROUPING) {
                    self.grouping(canAssign) catch |err| {
                        std.debug.print("ERROR: grouping(): {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                } else if (prefixRule == OperationType.UNARY) {
                    self.unary(canAssign) catch |err| {
                        std.debug.print("ERROR: unary(): {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                } else if (prefixRule == OperationType.NUMBER) {
                    self.number(canAssign) catch |err| {
                        std.debug.print("ERROR: number(): {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                } else if (prefixRule == OperationType.LITERAL) {
                    self.literal(canAssign) catch |err| {
                        std.debug.print("ERROR: literal(): {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                } else if (prefixRule == OperationType.STRING) {
                    self.string(canAssign) catch |err| {
                        std.debug.print("ERROR: string(): {}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
                } else if (prefixRule == OperationType.VARIABLE) {
                    self.variable(canAssign) catch |err| {
                        std.debug.print("ERROR: variable(): {}\n", .{err});
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
                            self.binary(canAssign) catch |err| {
                                std.debug.print("ERROR: {}\n", .{err});
                                return core.CompilerError.CompileError;
                            };
                        }

                        if (self.current) |ct| {
                            currentTokenRule = try ct.tokenType.getRule();
                        }
                    }

                    if (self.match(token.TokenType.EQUAL)) |isEqual| {
                        if (isEqual and canAssign) {
                            std.debug.print("ERROR: Cannot assign at {d}:{d}\n", .{ self.scanner.line, self.scanner.pos });
                            return core.CompilerError.UnexpectedToken;
                        }
                    } else |err| {
                        std.debug.print("ERROR: {?}\n", .{err});
                        return core.CompilerError.CompileError;
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

                    if (t.tokenType != token.TokenType.ERROR or t.tokenType == token.TokenType.EOF) {
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
                    return self.advance() catch |err| {
                        std.debug.print("ERROR: {?}\n", .{err});
                        return core.CompilerError.CompileError;
                    };
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
