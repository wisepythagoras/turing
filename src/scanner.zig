const std = @import("std");
const token = @import("token.zig");

pub fn Scanner() type {
    return struct {
        const Self = @This();

        source: []u8,
        line: usize,
        pos: usize,

        pub fn init(source: []u8) Self {
            return Self{
                .source = source,
                .line = 1,
                .pos = 0,
            };
        }

        pub fn scanToken(self: *Self) token.Token() {
            const pos = self.pos;
            self.pos += 1;

            if (pos == self.source.len) {
                return token.Token().init(token.TokenType.EOF, pos, self.line);
            }

            if (self.source[pos] == '\n') {
                self.line += 1;
            }

            return switch (self.source[pos]) {
                '(' => token.Token().init(token.TokenType.LEFT_PAREN, pos, self.line),
                ')' => token.Token().init(token.TokenType.RIGHT_PAREN, pos, self.line),
                '{' => token.Token().init(token.TokenType.LEFT_BRACE, pos, self.line),
                '}' => token.Token().init(token.TokenType.RIGHT_BRACE, pos, self.line),
                ',' => token.Token().init(token.TokenType.COMMA, pos, self.line),
                '.' => token.Token().init(token.TokenType.DOT, pos, self.line),
                '-' => token.Token().init(token.TokenType.MINUS, pos, self.line),
                '+' => token.Token().init(token.TokenType.PLUS, pos, self.line),
                '*' => token.Token().init(token.TokenType.STAR, pos, self.line),
                ';' => token.Token().init(token.TokenType.SEMICOLON, pos, self.line),
                '/' => token.Token().init(token.TokenType.SLASH, pos, self.line),
                else => token.Token().init(token.TokenType.IDENTIFIER, pos, self.line),
            };
        }
    };
}
