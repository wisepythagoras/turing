const std = @import("std");

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
    OUT, // Print to the screen
    POP, // Remove the last value from the stack
    DEFG, // Define a global variable
    GETG, // Get a global variable
    SETG, // Set the value of a global variable
    GETL, // Get a local variable
    SETL, // Set a local variable
    JWF, // Jump when/if false

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
        buf[0] = @as(u8, @intFromEnum(self));

        return buf;
    }

    pub fn fromU8(byte: u8) !OpCode {
        return std.meta.intToEnum(@This(), byte);
    }

    pub fn toU8(self: Self) u8 {
        return @as(u8, @intFromEnum(self));
    }
};
