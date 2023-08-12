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

        pub fn scanToken(self: *Self) token.Token() {
            self.skipWhitespace();
            const pos = self.pos;

            if (pos == self.source.len) {
                return token.Token().init(token.TokenType.EOF, pos, self.line);
            }

            // This will be updated further when encountering multi character tokens.
            self.pos += 1;

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
                '!' => {
                    if (pos < self.source.len - 1 and self.source[pos + 1] == '=') {
                        self.pos += 1;
                        return token.Token().init(token.TokenType.BANG_EQUAL, pos, self.line);
                    }

                    return token.Token().init(token.TokenType.BANG, pos, self.line);
                },
                '=' => {
                    if (pos < self.source.len - 1 and self.source[pos + 1] == '=') {
                        self.pos += 1;
                        return token.Token().init(token.TokenType.DOUBLE_EQUAL, pos, self.line);
                    }

                    return token.Token().init(token.TokenType.EQUAL, pos, self.line);
                },
                else => token.Token().init(token.TokenType.IDENTIFIER, pos, self.line),
            };
        }
    };
}
