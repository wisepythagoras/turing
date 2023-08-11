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

const tokenSizes: [39]usize = [39]usize{
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
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
        start: []u8,
        len: usize,
        line: usize,

        pub fn init(t: TokenType, start: []u8, line: usize, len: usize) Self {
            var tokenLen = len;

            if (tokenLen == 0) {
                tokenLen = tokenSizes[t.toUsize()];
            }

            return Self{
                .tokenType = t,
                .start = start,
                .len = tokenLen,
                .line = line,
            };
        }
    };
}
