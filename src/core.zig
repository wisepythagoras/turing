const std = @import("std");
const chunk = @import("chunk.zig");

pub const OpCode = enum(u8) {
    const Self = @This();

    RETURN,
    CONSTANT,
    CONSTANT_16,
    NEGATE,
    ADD,
    SUB,
    DIV,
    MUL,
    MOD,

    // https://ziglearn.org/chapter-2/#formatting
    pub fn toString(self: Self) []const u8 {
        const allocator = std.heap.page_allocator;

        if (std.fmt.allocPrint(allocator, "{?}", .{self})) |string| {
            return string;
        } else |err| {
            std.debug.print("{?}\n", .{err});
            return "";
        }
    }

    pub fn fromU8(byte: u8) !OpCode {
        return std.meta.intToEnum(@This(), byte);
        // return @as(OpCode, @enumFromInt(byte));
    }
};

pub const InterpretResults = enum(u8) {
    OK,
    CONTINUE,
    COMPILE_ERROR,
    RUNTIME_ERROR,
    SYNTAX_ERROR,
    UNEXPECTED_VALUE,
};

pub const ValueType = enum(u8) {
    NIL,
    BOOL,
    NUMBER,
    STRING,
};

pub const ValueUnion = union {
    number: f64,
    boolean: bool,
};

pub fn Value() type {
    return struct {
        const Self = @This();

        // number: f64,
        vType: ValueType,
        val: ValueUnion,

        pub fn initNumber(num: f64) Self {
            return Self{
                // .number = num,
                .val = ValueUnion{ .number = num },
                .vType = ValueType.NUMBER,
            };
        }

        pub fn initBool(b: bool) Self {
            return Self{
                .val = ValueUnion{ .boolean = b },
                .vType = ValueType.BOOL,
            };
        }

        pub fn initNil() Self {
            return Self{
                .val = ValueUnion{ .number = 0 },
                .vType = ValueType.NIL,
            };
        }

        pub fn isNumber(self: Self) bool {
            return self.vType == ValueType.NUMBER;
        }

        pub fn isNil(self: Self) bool {
            return self.vType == ValueType.NIL;
        }

        pub fn isBool(self: Self) bool {
            return self.vType == ValueType.BOOL;
        }

        pub fn print(self: Self) void {
            if (self.vType == ValueType.NUMBER) {
                std.debug.print("{d:.6}\n", .{self.val.number});
            }
        }
    };
}

pub const OperationFn = *const fn (Value(), Value()) CompilerError!Value();
pub const CompilerError = error{
    CompileError,
    RuntimeError,
    InvalidMemoryLookup,
    UnterminatedString,
    UnknownToken,
    SyntaxError,
    UnterminatedBlock,
    InvalidBlock,
    UnclosedParen,
    InvalidParen,
    UnexpectedToken,
    UninitializedStack,
    InvalidOperation,
    ExpectExpression,
};

pub fn addOp(a: Value(), b: Value()) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number + b.val.number);
}

pub fn subOp(a: Value(), b: Value()) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number - b.val.number);
}

pub fn mulOp(a: Value(), b: Value()) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number * b.val.number);
}

pub fn divOp(a: Value(), b: Value()) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number / b.val.number);
}

pub fn modOp(a: Value(), b: Value()) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number % b.val.number);
}

pub fn readConstant(c: *chunk.Chunk(), offset: usize) CompilerError!Value() {
    if (offset + 1 > c.code.items.len) {
        return CompilerError.InvalidMemoryLookup;
    }

    const idx: u8 = @intFromEnum(c.code.items[offset + 1][0]);
    return c.values.items[idx];
}

pub fn returnInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

pub fn constantInstruction(name: []const u8, c: *chunk.Chunk(), offset: usize) CompilerError!usize {
    if (readConstant(c, offset)) |constant| {
        if (constant.vType == ValueType.NUMBER) {
            const num = constant.val.number;
            std.debug.print("{s} = {x} ({d})\n", .{ name, num, num });
        }

        return offset + 2;
    } else |err| {
        return err;
    }
}

pub fn constant16Instruction(name: []const u8, c: *chunk.Chunk(), offset: usize) usize {
    const idxB: u16 = @intFromEnum(c.code.items[offset + 1][0]);
    const idxA: u16 = @intFromEnum(c.code.items[offset + 2][0]);
    const idx: u16 = (idxB << 8) | idxA;

    const constant: Value() = c.values.items[idx];

    if (constant.vType == ValueType.NUMBER) {
        std.debug.print("{s} = {x} ({d})\n", .{ name, idx, constant.val.number });
    }

    return offset + 2;
}

pub fn simpleInstruction(name: []const u8, offset: usize) CompilerError!usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
