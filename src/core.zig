const std = @import("std");
const chunk = @import("chunk.zig");
const object = @import("object.zig");

pub const OpCode = enum(u8) {
    const Self = @This();

    RETURN,
    CONSTANT,
    CONSTANT_16,
    NEG,
    ADD,
    SUB,
    DIV,
    MUL,
    MOD,
    NIL,
    TRUE,
    FALSE,
    XOR,
    POW,
    AND,
    NOT,
    EQ, // Equal
    NE, // Not equal
    GT, // Greater than
    GE, // Greater or equal
    LT, // Less than
    LE, // Less or equal

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

    pub fn toBytes(self: Self) ![]const u8 {
        const memory = std.heap.page_allocator;
        const buf = try memory.alloc(u8, 1);
        buf[0] = @as(u8, self);

        return buf;
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
    OBJECT,
};

pub const ValueUnion = union {
    number: f64,
    boolean: bool,
    object: *object.Object(),
    bytes: [8]u8,
};

pub fn Value() type {
    return struct {
        const Self = @This();

        // number: f64,
        vType: ValueType,
        val: ValueUnion,

        pub fn init(val: anytype) ?Self {
            if (@TypeOf(val) == bool) {
                return Self{
                    .val = ValueUnion{ .boolean = val },
                    .vType = ValueType.BOOL,
                };
            } else if (@TypeOf(val) == f64) {
                return Self{
                    .val = ValueUnion{ .number = val },
                    .vType = ValueType.NUMBER,
                };
            } else if (@TypeOf(val) == object.Object()) {
                return Self{
                    .val = ValueUnion{ .object = val },
                    .vType = ValueType.OBJECT,
                };
            }

            return null;
        }

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

        pub fn initObj(obj: *object.Object()) Self {
            return Self{
                .val = ValueUnion{ .object = obj },
                .vType = ValueType.OBJECT,
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

        pub fn destroy(self: Self) bool {
            if (self.vType == ValueType.OBJECT) {
                const memory = std.heap.page_allocator;
                memory.destroy(self.val.object);

                return true;
            }

            return false;
        }

        pub fn print(self: Self) void {
            if (self.vType == ValueType.NUMBER) {
                std.debug.print("{d:.6}\n", .{self.val.number});
            } else if (self.vType == ValueType.BOOL) {
                var valToPrint: []const u8 = "true";

                if (!self.val.boolean) {
                    valToPrint = "false";
                }

                std.debug.print("{s}\n", .{valToPrint});
            } else if (self.vType == ValueType.NIL) {
                std.debug.print("nil\n", .{});
            } else if (self.vType == ValueType.OBJECT) {
                std.debug.print("{s}\n", .{self.toString()});
            }
        }

        pub fn toString(self: Self) []const u8 {
            if (self.vType == ValueType.OBJECT) {
                return self.val.object.toString();
            }

            if (self.vType == ValueType.BOOL) {
                return if (self.val.boolean) "true" else "false";
            }

            var buf: [256]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "{}", .{self.val.number}) catch {
                return "";
            };

            return str;
        }

        /// Convert the value to a byte array.
        pub fn toBytes(self: Self) []const u8 {
            if (self.vType == ValueType.OBJECT) {
                return self.val.object.toString();
            }

            if (self.vType == ValueType.BOOL) {
                return [_]u8{if (self.val.boolean) 1 else 0};
            }

            return self.val.bytes;
        }
    };
}

pub const OperationFn = *const fn (Value(), Value(), ?OpCode) CompilerError!Value();
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
    MemoryError,
};

pub fn addOp(a: Value(), b: Value(), _: ?OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        if ((a.vType == ValueType.OBJECT and a.val.object.objType != object.ObjectType.STRING) or
            (b.vType == ValueType.OBJECT and b.val.object.objType != object.ObjectType.STRING))
        {
            return CompilerError.RuntimeError;
        }
    }

    if (a.vType == ValueType.NUMBER and b.vType == ValueType.NUMBER) {
        return Value().initNumber(a.val.number + b.val.number);
    }

    const aStr = a.toString();
    const bStr = b.toString();
    _ = a.destroy();
    _ = b.destroy();
    const memory = std.heap.page_allocator;

    const buf = memory.alloc(u8, aStr.len + bStr.len) catch {
        return CompilerError.MemoryError;
    };

    const str = std.fmt.bufPrint(buf, "{s}{s}", .{ aStr, bStr }) catch {
        return CompilerError.RuntimeError;
    };
    const strObj = object.String().init(str);

    if (object.Object().init(strObj)) |obj| {
        const o = memory.create(object.Object()) catch {
            return CompilerError.MemoryError;
        };
        o.* = obj;

        return Value().initObj(o);
    }

    return Value().initNil();
}

pub fn subOp(a: Value(), b: Value(), _: ?OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number - b.val.number);
}

pub fn mulOp(a: Value(), b: Value(), _: ?OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number * b.val.number);
}

pub fn divOp(a: Value(), b: Value(), _: ?OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number / b.val.number);
}

pub fn modOp(a: Value(), b: Value(), _: ?OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number % b.val.number);
}

pub fn powOp(a: Value(), b: Value(), _: ?OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    const res: f64 = std.math.pow(f64, a.val.number, b.val.number);

    return Value().initNumber(res);
}

pub fn andOp(a: Value(), b: Value(), _: ?OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    const numA: u16 = @as(u16, @intFromFloat(a.val.number));
    const numB: u16 = @as(u16, @intFromFloat(b.val.number));
    const res: f64 = @as(f64, @floatFromInt(numA & numB));

    return Value().initNumber(res);
}

pub fn xorOp(a: Value(), b: Value(), _: ?OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    const numA: u16 = @as(u16, @intFromFloat(a.val.number));
    const numB: u16 = @as(u16, @intFromFloat(b.val.number));
    const res: f64 = @as(f64, @floatFromInt(numA ^ numB));

    return Value().initNumber(res);
}

pub fn eqOp(a: Value(), b: Value(), ins: ?OpCode) !Value() {
    if ((a.vType == ValueType.OBJECT and b.vType != ValueType.OBJECT) or
        (a.vType != ValueType.OBJECT and b.vType == ValueType.OBJECT))
    {
        if (Value().init(false)) |val| {
            return val;
        }
    } else if (a.vType == ValueType.OBJECT and b.vType == ValueType.OBJECT) {
        if (a.val.object.objType == object.ObjectType.STRING) {
            return Value().initBool(a.val.object.isEqual(b.val.object));
        }
    }

    var aVal: f64 = 0.0;
    var bVal: f64 = 0.0;

    if (a.vType == ValueType.NUMBER) {
        aVal = a.val.number;
    } else if (a.vType == ValueType.BOOL) {
        aVal = if (a.val.boolean) 1.0 else 0.0;
    }

    if (b.vType == ValueType.NUMBER) {
        bVal = b.val.number;
    } else if (b.vType == ValueType.BOOL) {
        bVal = if (b.val.boolean) 1.0 else 0.0;
    }

    if (ins) |instruction| {
        if (instruction == OpCode.EQ) {
            return Value().initBool(aVal == bVal);
        } else if (instruction == OpCode.NE) {
            return Value().initBool(aVal != bVal);
        } else if (instruction == OpCode.GT) {
            return Value().initBool(aVal > bVal);
        } else if (instruction == OpCode.GE) {
            return Value().initBool(aVal >= bVal);
        } else if (instruction == OpCode.LT) {
            return Value().initBool(aVal < bVal);
        } else if (instruction == OpCode.LE) {
            return Value().initBool(aVal <= bVal);
        }
    }

    return Value().initBool(false);
}

pub fn isFalsey(a: Value()) bool {
    const isNil = a.vType == ValueType.NIL;
    const isZero = a.vType == ValueType.NUMBER and a.val.number == @as(f64, 0);
    const isFalse = a.vType == ValueType.BOOL and !a.val.boolean;
    // TODO: If a string is empty then it's falsey.

    return isNil or isZero or isFalse;
}

pub fn readValue(c: *chunk.Chunk(), offset: usize) CompilerError!Value() {
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

pub fn nilInstruction(name: []const u8, c: *chunk.Chunk(), offset: usize) CompilerError!usize {
    if (readValue(c, offset)) |constant| {
        if (constant.vType == ValueType.NIL) {
            std.debug.print("{s} = nil\n", .{name});
        }

        return offset + 1;
    } else |err| {
        return err;
    }
}

pub fn booleanInstruction(name: []const u8, c: *chunk.Chunk(), offset: usize) CompilerError!usize {
    if (readValue(c, offset)) |constant| {
        if (constant.vType == ValueType.BOOL) {
            const b = constant.val.boolean;
            std.debug.print("{s} = {?}\n", .{ name, b });
        }

        return offset + 1;
    } else |err| {
        return err;
    }
}

pub fn constantInstruction(name: []const u8, c: *chunk.Chunk(), offset: usize) CompilerError!usize {
    if (readValue(c, offset)) |constant| {
        if (constant.vType == ValueType.NUMBER) {
            const num = constant.val.number;
            std.debug.print("{s} = {x} ({d})\n", .{ name, num, num });
        } else if (constant.vType == ValueType.OBJECT) {
            const obj = constant.val.object;

            if (obj.objType == object.ObjectType.STRING) {
                const str = obj.val.string.chars;
                std.debug.print("{s} = \"{s}\"\n", .{ name, str });
            }
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
