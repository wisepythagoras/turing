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

        fn skipWhitespace(self: *Self) void {
            while (self.pos < self.source.len) {
                switch (self.source[self.pos]) {
                    ' ', '\r', '\t' => self.pos += 1,
                    '\n' => {
                        self.line += 1;
                        self.pos += 1;
                    },
                    else => break,
                }
            }
        }

        fn singleOrDouble(self: *Self, two: u8, a: token.TokenType, b: token.TokenType) token.Token() {
            if (self.pos < self.source.len - 1 and self.source[self.pos + 1] == two) {
                self.pos += 1;
                return token.Token().init(b, self.pos - 1, self.line);
            }

            return token.Token().init(a, self.pos, self.line);
        }

        pub fn scanToken(self: *Self) token.Token() {
            self.skipWhitespace();
            const pos = self.pos;

            if (pos == self.source.len) {
                return token.Token().init(token.TokenType.EOF, pos, self.line);
            }

            var newToken = switch (self.source[pos]) {
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
                '!' => self.singleOrDouble('=', token.TokenType.BANG, token.TokenType.BANG_EQUAL),
                '=' => self.singleOrDouble('=', token.TokenType.EQUAL, token.TokenType.DOUBLE_EQUAL),
                '>' => self.singleOrDouble('=', token.TokenType.GREATER_THAN, token.TokenType.GREATER_EQUAL_THAN),
                '<' => self.singleOrDouble('=', token.TokenType.LESS_THAN, token.TokenType.LESS_EQUAL_THAN),
                else => token.Token().init(token.TokenType.IDENTIFIER, pos, self.line),
            };

            // This will be updated further when encountering multi character tokens.
            self.pos += 1;

            return newToken;
        }
    };
}
