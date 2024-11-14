const std = @import("std");

pub const OpCode = enum(u8) {
    const Self = @This();

    RETURN,
    CONSTANT,
    CONSTANT_16,
    NEG, // Negate (3)
    ADD, // Add (4)
    SUB, // Subtract (5)
    DIV, // Divide (6)
    MUL, // Multiply (7)
    MOD, // Modulo (8)
    NIL, // Nil/null (9)
    TRUE, // True (10)
    FALSE, // False (11)
    XOR, // Exclusive or (12)
    POW, // Power (13)
    AND, // Logic and (14)
    NOT, // Not (15)
    EQ, // Equal (16)
    NE, // Not equal (17)
    GT, // Greater than (18)
    GE, // Greater or equal (19)
    LT, // Less than (20)
    LE, // Less or equal (21)
    OUT, // Print to the screen (22)
    POP, // Remove the last value from the stack (23)
    DEFG, // Define a global variable (24)
    GETG, // Get a global variable (25)
    SETG, // Set the value of a global variable (26)
    GETL, // Get a local variable (27)
    SETL, // Set a local variable (28)
    JWF, // Jump when/if false (29)
    JMP, // Simple jump (30)
    LOOP, // Simple loop back instruction (31)

    // https://ziglearn.org/chapter-2/#formatting
    pub fn toString(self: Self) []const u8 {
        const allocator = std.heap.c_allocator;

        if (std.fmt.allocPrint(allocator, "{?}", .{self})) |string| {
            return string;
        } else |err| {
            std.debug.print("{?}\n", .{err});
            return "";
        }
    }

    pub fn toBytes(self: Self) ![]const u8 {
        const memory = std.heap.c_allocator;
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
