const std = @import("std");

pub const TokenType = enum(u8) {
    const Self = @This();

    // Single character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    STAR,
    SEMICOLON,
    SLASH,
    BANG,
    EQUAL,
    GREATER_THAN,
    LESS_THAN,
    COMMENT,

    // Multi-character tokens.
    BANG_EQUAL,
    DOUBLE_EQUAL,
    GREATER_EQUAL_THAN,
    LESS_EQUAL_THAN,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    OR,
    STRUCT, // Class equivalent
    IF,
    ELSE,
    FALSE,
    TRUE,
    FOR,
    WHILE,
    FUNCTION,
    RETURN,
    SUPER,
    THIS,
    VAR,
    CONST,

    // Misc.
    ERROR,
    EOF,

    pub fn toUsize(self: Self) usize {
        return @as(usize, @intFromEnum(self));
    }
};

const tokenSizes: [40]usize = [40]usize{
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2,
    0, 0, 0, // The literals, because they're dynamic
    3, 2, // And and or
    6, // Stuct
    5, 4, // False and true
    2, 4, // if-else
    3, 5, // for and while
    3, // Function
    6, // Return
    5, // Super
    4, // This
    3, 4, // Var and const
    0, 0,
};

pub fn Token() type {
    return struct {
        const Self = @This();

        tokenType: TokenType,
        pos: usize,
        line: usize,

        pub fn init(t: TokenType, pos: usize, line: usize) Self {
            return Self{
                .tokenType = t,
                .pos = pos,
                .line = line,
            };
        }

        pub fn toString(self: Self, source: []u8) ![]u8 {
            var len = tokenSizes[self.tokenType.toUsize()];
            const allocator = std.heap.page_allocator;

            // TODO: This is temporary.
            if (len == 0) {
                len = 1;
            }

            if (allocator.alloc(u8, len)) |buf| {
                if (len == 1) {
                    buf[0] = source[self.pos];
                    return buf;
                }

                var i: usize = 0;

                while (i < len) {
                    buf[i] = source[i + self.pos];
                    i += 1;
                }

                return buf;
            } else |err| {
                return err;
            }
        }
    };
}
