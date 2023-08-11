const std = @import("std");
const token = @import("token.zig");

pub fn Scanner() type {
    return struct {
        const Self = @This();

        start: []u8,
        current: []u8,
        line: usize,
        pos: usize,

        pub fn init(source: []u8) Self {
            return Self{
                .start = source,
                .current = source,
                .line = 1,
                .pos = 0,
            };
        }

        pub fn scanToken(self: *Self) token.Token() {
            const pos = self.pos;
            self.pos += 1;

            if (pos == self.start.len) {
                return token.Token().init(token.TokenType.EOF, self.start, self.line, 0);
            }

            return token.Token().init(token.TokenType.IDENTIFIER, self.start, self.line, 0);
        }
    };
}
