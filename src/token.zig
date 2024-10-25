const std = @import("std");
const parser = @import("parser.zig");
const core = @import("core.zig");

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
    SEMICOLON,
    STAR,
    SLASH,
    BANG,
    EQUAL,
    GREATER_THAN,
    LESS_THAN,
    CARET,
    PERCENT,
    AMPERSAND,
    COMMENT,

    // Multi-character tokens.
    BANG_EQUAL,
    DOUBLE_EQUAL,
    GREATER_EQUAL_THAN,
    LESS_EQUAL_THAN,
    STAR_STAR,

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
    NIL,
    PRINT,

    // Misc.
    ERROR,
    EOF,

    /// Gets the numerical representation of the enum value.
    pub fn toUsize(self: Self) usize {
        return @as(usize, @intFromEnum(self));
    }

    /// Returns the appropriate parser rule
    pub fn getRule(self: Self) !parser.Rule {
        const idx = self.toUsize();

        if (idx < 0 or idx >= tokenSizes.len) {
            return core.CompilerError.InvalidOperation;
        }

        return parser.ParseRules[idx];
    }
};

const tokenSizes: [46]usize = [46]usize{
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2,
    0, 0, 0, // The literals, because they're dynamic
    3, 2, // And and or
    6, // Stuct
    2, 4, // if-else
    5, 4, // False and true
    3, 5, // for and while
    3, // Function
    6, // Return
    5, // Super
    4, // This
    3, 4, // Var (let) and const
    3, // Nil
    5, // Print
    0,
    0,
};

pub fn Token() type {
    return struct {
        const Self = @This();

        tokenType: TokenType,
        pos: usize,
        // line: usize,
        size: usize,

        pub fn init(t: TokenType, pos: usize, line: usize) Self {
            _ = line;
            return Self{
                .tokenType = t,
                .pos = pos,
                // .line = line,
                .size = 0,
            };
        }

        pub fn initWithSize(t: TokenType, pos: usize, size: usize) Self {
            return Self{
                .tokenType = t,
                .pos = pos,
                // .line = line,
                .size = size,
            };
        }

        pub fn toString(self: Self, source: []u8) ![]u8 {
            var len = tokenSizes[self.tokenType.toUsize()];
            const allocator = std.heap.page_allocator;

            if (len == 0) {
                // TODO: This is temporary.
                if (self.tokenType == TokenType.EOF or self.tokenType == TokenType.ERROR) {
                    len = 1;
                } else {
                    len = self.size;
                }
            }

            if (allocator.alloc(u8, len)) |buf| {
                if (self.pos >= source.len) {
                    buf[0] = 0;
                    return buf;
                }

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
