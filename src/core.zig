const std = @import("std");
const chunk = @import("chunk.zig");
const object = @import("object.zig");
const opcode = @import("opcode.zig");
const utils = @import("utils.zig");
const cLib = @cImport({
    @cDefine("_NO_CRT_STDIO_INLINE", "1");
    @cInclude("stdio.h");
    @cInclude("stdlib.h");
});

pub const InterpretResults = enum(u8) {
    OK,
    CONTINUE,
    COMPILE_ERROR,
    RUNTIME_ERROR,
    SYNTAX_ERROR,
    UNEXPECTED_VALUE,
};

pub const ValueType = enum(u8) {
    const Self = @This();

    NIL,
    BOOL,
    NUMBER,
    OBJECT,

    pub fn toU8(self: Self) u8 {
        return @as(u8, @intFromEnum(self));
    }
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

        pub fn isString(self: Self) bool {
            if (self.vType != ValueType.OBJECT) {
                return false;
            }

            return self.val.object.objType == object.ObjectType.STRING;
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
                const a = @floor(self.val.number);

                if (a != self.val.number) {
                    _ = cLib.printf("%f\n", self.val.number);
                } else {
                    _ = cLib.printf("%.0f\n", self.val.number);
                }
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

            // TODO: This seems to be wasteful and it underperforms.
            const memory = std.heap.page_allocator;
            const floored = @floor(self.val.number);
            var str: []u8 = undefined;

            if (floored != self.val.number) {
                str = std.fmt.allocPrint(memory, "{d:.6}", .{self.val.number}) catch {
                    return "";
                };
            } else {
                str = std.fmt.allocPrint(memory, "{d}", .{@as(i64, @intFromFloat(self.val.number))}) catch {
                    return "";
                };
            }

            return str;
        }

        /// Convert the value to a byte array.
        pub fn toBytes(self: Self) ![]const u8 {
            if (self.vType == ValueType.OBJECT) {
                return self.val.object.toBytes() catch |err| {
                    std.debug.print("ERROR: {?} (toBytes internal)\n", .{err});
                    return CompilerError.MemoryError; // TODO: Handle this better.
                };
            }

            const memory = std.heap.page_allocator;

            if (self.vType == ValueType.BOOL) {
                const res = memory.alloc(u8, 1) catch |err| {
                    std.debug.print("ERROR: {?} (value alloc)\n", .{err});
                    return CompilerError.MemoryError;
                };

                res[0] = if (self.val.boolean) 1 else 0;
                return res;
            }

            const res = memory.alloc(u8, 8) catch |err| {
                std.debug.print("ERROR: {?} (value alloc)\n", .{err});
                return CompilerError.MemoryError;
            };

            const floatBytes: [8]u8 = @bitCast(self.val.number);
            var i: usize = 0;

            for (floatBytes) |b| {
                res[i] = b;
                i += 1;
            }

            return res;
        }
    };
}

pub const OperationFn = *const fn (Value(), Value(), ?opcode.OpCode) CompilerError!Value();
pub const GetterFn = *const fn (*chunk.Chunk(), usize) CompilerError!Value();
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
    Redeclaration,
};

/// TODO: This function seems to be underperforming.
pub fn addOp(a: Value(), b: Value(), _: ?opcode.OpCode) !Value() {
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

    // Free the memory.
    // TODO: Garbage collecting here will get in the way of reusing constants within a loop. Or
    // Even in a case where we reuse the constant for some operation. So garbage collection
    // needs to be done at the end of each block.
    //_ = a.destroy();
    //_ = b.destroy();

    const memory = std.heap.page_allocator;

    const buf = memory.alloc(u8, aStr.len + bStr.len) catch {
        return CompilerError.MemoryError;
    };

    const str = std.fmt.bufPrint(buf, "{s}{s}", .{ aStr, bStr }) catch {
        return CompilerError.RuntimeError;
    };

    return utils.strToObject(str) catch |err| {
        if (err == CompilerError.RuntimeError) {
            return CompilerError.RuntimeError;
        }

        return CompilerError.MemoryError;
    };
}

pub fn subOp(a: Value(), b: Value(), _: ?opcode.OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number - b.val.number);
}

pub fn mulOp(a: Value(), b: Value(), _: ?opcode.OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number * b.val.number);
}

pub fn divOp(a: Value(), b: Value(), _: ?opcode.OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number / b.val.number);
}

pub fn modOp(a: Value(), b: Value(), _: ?opcode.OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    return Value().initNumber(a.val.number % b.val.number);
}

pub fn powOp(a: Value(), b: Value(), _: ?opcode.OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    const res: f64 = std.math.pow(f64, a.val.number, b.val.number);

    return Value().initNumber(res);
}

pub fn andOp(a: Value(), b: Value(), _: ?opcode.OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    const numA: u16 = @as(u16, @intFromFloat(a.val.number));
    const numB: u16 = @as(u16, @intFromFloat(b.val.number));
    const res: f64 = @as(f64, @floatFromInt(numA & numB));

    return Value().initNumber(res);
}

pub fn xorOp(a: Value(), b: Value(), _: ?opcode.OpCode) !Value() {
    if (a.vType != ValueType.NUMBER or b.vType != ValueType.NUMBER) {
        return CompilerError.RuntimeError;
    }

    const numA: u16 = @as(u16, @intFromFloat(a.val.number));
    const numB: u16 = @as(u16, @intFromFloat(b.val.number));
    const res: f64 = @as(f64, @floatFromInt(numA ^ numB));

    return Value().initNumber(res);
}

pub fn eqOp(a: Value(), b: Value(), ins: ?opcode.OpCode) !Value() {
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
        if (instruction == opcode.OpCode.EQ) {
            return Value().initBool(aVal == bVal);
        } else if (instruction == opcode.OpCode.NE) {
            return Value().initBool(aVal != bVal);
        } else if (instruction == opcode.OpCode.GT) {
            return Value().initBool(aVal > bVal);
        } else if (instruction == opcode.OpCode.GE) {
            return Value().initBool(aVal >= bVal);
        } else if (instruction == opcode.OpCode.LT) {
            return Value().initBool(aVal < bVal);
        } else if (instruction == opcode.OpCode.LE) {
            return Value().initBool(aVal <= bVal);
        }
    }

    return Value().initBool(false);
}

pub fn isFalsey(a: Value()) bool {
    const isNil = a.vType == ValueType.NIL;
    const isZero = a.vType == ValueType.NUMBER and a.val.number == @as(f64, 0);
    const isFalse = a.vType == ValueType.BOOL and !a.val.boolean;
    const isEmptyString = a.isString() and a.val.object.val.string.len == 0;

    return isNil or isZero or isFalse or isEmptyString;
}

pub fn readValueIdx(c: *chunk.Chunk(), offset: usize) CompilerError!u8 {
    if (offset + 1 > c.code.items.len) {
        return CompilerError.InvalidMemoryLookup;
    }

    const idx: u8 = c.code.items[offset + 1][0];
    return idx;
}

pub fn readRaw16(c: *chunk.Chunk(), offset: usize) CompilerError!usize {
    const idxB: u16 = try readValueIdx(c, offset);
    const idxA: u16 = try readValueIdx(c, offset + 1);
    return (idxB << 8) | idxA;
}

pub fn readRaw32(c: *chunk.Chunk(), offset: usize) CompilerError!usize {
    const idxD: u32 = try readValueIdx(c, offset);
    const idxC: u32 = try readValueIdx(c, offset + 1);
    const idxB: u32 = try readValueIdx(c, offset + 2);
    const idxA: u32 = try readValueIdx(c, offset + 3);
    return (idxD << 24) | (idxC << 16) | (idxB << 8) | idxA;
}

pub fn readValue(c: *chunk.Chunk(), offset: usize) CompilerError!Value() {
    const idx = try readValueIdx(c, offset);
    return c.values.items[idx];
}

pub fn readValue16(c: *chunk.Chunk(), offset: usize) CompilerError!Value() {
    const idx = try readRaw16(c, offset);
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

pub fn valuesToBytes(c: *chunk.Chunk()) CompilerError![]const u8 {
    const memory = std.heap.page_allocator;
    var res = memory.alloc(u8, 0) catch |err| {
        std.debug.print("ERROR: {?} (alloc)\n", .{err});
        return CompilerError.MemoryError;
    };

    var i: usize = 0;
    var len: usize = 0;

    for (c.values.items) |value| {
        const val = try value.toBytes();
        len = len + val.len + 2;

        res = memory.realloc(res, len) catch |err| {
            std.debug.print("ERROR: {?} (realloc)\n", .{err});
            return CompilerError.MemoryError;
        };

        res[i] = @as(u8, @intFromEnum(value.vType));
        i += 1;

        for (val) |b| {
            res[i] = b;
            i += 1;
        }

        res[i] = 0;
        i += 1;
    }

    return res;
}

pub fn constToBytes(c: *chunk.Chunk(), opCode: opcode.OpCode, offset: *usize) CompilerError![]const u8 {
    const is16 = opCode == opcode.OpCode.CONSTANT_16;
    const idx = try readValueIdx(c, offset.*);
    const idx2: u8 = if (is16) try readValueIdx(c, offset.* + 1) else 0;
    const size: usize = if (is16) 3 else 2;
    offset.* += size;

    const memory = std.heap.page_allocator;
    const res = memory.alloc(u8, size) catch |err| {
        std.debug.print("ERROR: {?} (alloc)\n", .{err});
        return CompilerError.MemoryError;
    };
    const opCodeBytes = opCode.toBytes() catch |err| {
        std.debug.print("ERROR: {?} (realloc)\n", .{err});
        return CompilerError.MemoryError;
    };

    res[0] = opCodeBytes[0];
    res[1] = idx;

    if (is16) {
        res[2] = idx2;
    }

    return res;
}

pub fn varInstruction(name: []const u8, c: *chunk.Chunk(), offset: usize) CompilerError!usize {
    const size = c.code.items[offset + 1][0];
    var targetGetter: GetterFn = readValue;
    var idx: usize = undefined;

    if (size == opcode.OpCode.CONSTANT_16.toU8()) {
        targetGetter = readValue16;
        idx = try readRaw16(c, offset + 1);
    } else {
        idx = try readValueIdx(c, offset + 1);
    }

    if (targetGetter(c, offset + 1)) |constant| {
        if (constant.vType == ValueType.NUMBER) {
            const num = constant.val.number;
            std.debug.print("{s} = {x} ({d}) @{d}\n", .{ name, num, num, idx });
        } else if (constant.vType == ValueType.OBJECT) {
            const obj = constant.val.object;

            if (obj.objType == object.ObjectType.STRING) {
                const str = obj.val.string.chars;
                std.debug.print("{s} = \"{s}\" @{d}\n", .{ name, str, idx });
            }
        }

        const newOffset: usize = if (size == 1) 3 else 4;
        return offset + newOffset;
    } else |err| {
        return err;
    }
}

pub fn branchInstruction(name: []const u8, c: *chunk.Chunk(), offset: usize) CompilerError!usize {
    const idx = try readRaw32(c, offset + 1);
    std.debug.print("{s} -> @{d}\n", .{ name, idx });
    return offset + 5;
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

pub fn constant16Instruction(name: []const u8, c: *chunk.Chunk(), offset: usize) !usize {
    const constant = try readValue16(c, offset);

    if (constant.vType == ValueType.NUMBER) {
        std.debug.print("{s} = {x} ({d})\n", .{ name, constant.val.number, constant.val.number });
    } else if (constant.vType == ValueType.OBJECT) {
        const obj = constant.val.object;

        if (obj.objType == object.ObjectType.STRING) {
            const str = obj.val.string.chars;
            std.debug.print("{s} = \"{s}\"\n", .{ name, str });
        }
    }

    return offset + 3;
}

pub fn simpleInstruction(name: []const u8, offset: usize) CompilerError!usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
