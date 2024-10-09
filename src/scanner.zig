const std = @import("std");
const token = @import("token.zig");
const core = @import("core.zig");
const utils = @import("utils.zig");

pub fn Scanner() type {
    return struct {
        const Self = @This();

        source: []u8,
        line: usize,
        pos: usize,
        parenLevel: usize,
        braceLevel: usize,

        pub fn init(source: []u8) Self {
            return Self{
                .source = source,
                .line = 1,
                .pos = 0,
                .parenLevel = 0,
                .braceLevel = 0,
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

        fn skipToNewLine(self: *Self) void {
            while (self.pos < self.source.len) {
                if (self.source[self.pos] == '\n') {
                    self.pos += 1;
                    self.line += 1;
                    break;
                }

                self.pos += 1;
            }
        }

        fn findEndQuote(self: *Self, kind: u8) core.CompilerError!usize {
            var pos = self.pos + 1;

            while (pos < self.source.len) {
                if (self.source[pos] == kind) {
                    return pos;
                }

                pos += 1;
            }

            return core.CompilerError.UnterminatedString;
        }

        fn singleOrDouble(self: *Self, two: u8, a: token.TokenType, b: token.TokenType) token.Token() {
            if (self.pos < self.source.len - 1 and self.source[self.pos + 1] == two) {
                self.pos += 1;
                return token.Token().init(b, self.pos - 1, self.line);
            }

            return token.Token().init(a, self.pos, self.line);
        }

        /// Checks if the next token is a digit and returns its size. If it's not a digit it returns 0.
        fn getDigitEndPos(self: Self) usize {
            var pos = self.pos;

            while (pos < self.source.len and
                utils.isDigit(self.source[pos], true))
            {
                pos += 1;
            }

            if (pos == self.pos) {
                return 0;
            }

            return pos;
        }

        fn getAlphanumEndPos(self: Self) usize {
            var pos = self.pos;

            if (!utils.isAlpha(self.source[pos])) {
                return 0;
            }

            while (pos < self.source.len and utils.isAlphaNum(self.source[pos])) {
                pos += 1;
            }

            return pos;
        }

        pub fn scanToken(self: *Self) core.CompilerError!token.Token() {
            self.skipWhitespace();

            if (self.pos >= self.source.len) {
                if (self.parenLevel > 0) {
                    return core.CompilerError.UnclosedParen;
                } else if (self.braceLevel > 0) {
                    return core.CompilerError.UnterminatedBlock;
                }

                return token.Token().init(token.TokenType.EOF, self.pos, self.line);
            }

            // This is a single-line comment.
            if (self.source[self.pos] == '#') {
                self.skipToNewLine();
            }

            // Here we check for a string.
            else if (self.source[self.pos] == '\'' or self.source[self.pos] == '"') {
                if (self.findEndQuote(self.source[self.pos])) |endPos| {
                    const stringToken = token.Token().initWithSize(
                        token.TokenType.STRING,
                        self.pos + 1,
                        endPos - self.pos - 1,
                    );
                    self.pos = endPos + 1;

                    return stringToken;
                } else |err| {
                    return err;
                }
            }

            // Now let's check for numbers.
            const digitEndPos = self.getDigitEndPos();

            if (digitEndPos > 0) {
                const numberToken = token.Token().initWithSize(
                    token.TokenType.NUMBER,
                    self.pos,
                    digitEndPos - self.pos,
                );
                self.pos = digitEndPos;

                return numberToken;
            }

            // Now let's check for identifiers.
            const alphaNumEndPos = self.getAlphanumEndPos();

            if (alphaNumEndPos > 0) {
                var identifierToken = token.Token().initWithSize(
                    token.TokenType.IDENTIFIER,
                    self.pos,
                    alphaNumEndPos - self.pos,
                );

                if (identifierToken.toString(self.source)) |str| {
                    var newTokenType = token.TokenType.IDENTIFIER;

                    // TODO: The following is not efficient, so we're going to need to do some extra
                    // work here to do these comparisons only when they're necessary.
                    if (str.len == 3 and utils.strcomp(str, "and")) {
                        newTokenType = token.TokenType.AND;
                    } else if (str.len == 2 and utils.strcomp(str, "or")) {
                        newTokenType = token.TokenType.OR;
                    } else if (str.len == 3 and utils.strcomp(str, "for")) {
                        newTokenType = token.TokenType.FOR;
                    } else if (str.len == 5 and utils.strcomp(str, "while")) {
                        newTokenType = token.TokenType.WHILE;
                    } else if (str.len == 2 and utils.strcomp(str, "if")) {
                        newTokenType = token.TokenType.IF;
                    } else if (str.len == 4 and utils.strcomp(str, "else")) {
                        newTokenType = token.TokenType.ELSE;
                    } else if (str.len == 4 and utils.strcomp(str, "true")) {
                        newTokenType = token.TokenType.TRUE;
                    } else if (str.len == 5 and utils.strcomp(str, "false")) {
                        newTokenType = token.TokenType.FALSE;
                    } else if (str.len == 3 and utils.strcomp(str, "nil")) {
                        newTokenType = token.TokenType.NIL;
                    } else if (str.len == 3 and utils.strcomp(str, "fun")) {
                        newTokenType = token.TokenType.FUNCTION;
                    } else if (str.len == 4 and utils.strcomp(str, "this")) {
                        newTokenType = token.TokenType.THIS;
                    } else if (str.len == 3 and utils.strcomp(str, "nil")) {
                        newTokenType = token.TokenType.NIL;
                    } else if (str.len == 4 and utils.strcomp(str, "super")) {
                        newTokenType = token.TokenType.SUPER;
                    } else if (str.len == 5 and utils.strcomp(str, "const")) {
                        newTokenType = token.TokenType.CONST;
                    } else if (str.len == 3 and utils.strcomp(str, "let")) {
                        newTokenType = token.TokenType.VAR;
                    } else if (str.len == 6 and utils.strcomp(str, "return")) {
                        newTokenType = token.TokenType.RETURN;
                    } else if (str.len == 6 and utils.strcomp(str, "struct")) {
                        newTokenType = token.TokenType.STRUCT;
                    }

                    // TODO: Add everything here.

                    identifierToken.tokenType = newTokenType;
                } else |err| {
                    std.debug.print("ERROR: {?}\n", .{err});
                    return core.CompilerError.CompileError;
                }

                self.pos = alphaNumEndPos;

                return identifierToken;
            }

            const pos = self.pos;
            const newToken = switch (self.source[pos]) {
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
                else => token.Token().init(token.TokenType.ERROR, pos, self.line),
            };

            if (newToken.tokenType == token.TokenType.ERROR) {
                return core.CompilerError.UnknownToken;
            } else if (newToken.tokenType == token.TokenType.LEFT_PAREN) {
                self.parenLevel += 1;
            } else if (newToken.tokenType == token.TokenType.RIGHT_PAREN) {
                if (self.parenLevel == 0) {
                    return core.CompilerError.InvalidParen;
                }

                std.debug.print("Down the brace level -> {}\n", .{self.braceLevel});
                self.parenLevel -= 1;
            } else if (newToken.tokenType == token.TokenType.LEFT_BRACE) {
                std.debug.print("Up the brace level -> {}\n", .{self.braceLevel});
                self.braceLevel += 1;
            } else if (newToken.tokenType == token.TokenType.RIGHT_BRACE) {
                if (self.parenLevel == 0) {
                    return core.CompilerError.InvalidBlock;
                }

                std.debug.print("Down the brace level -> {}\n", .{self.braceLevel});
                self.braceLevel -= 1;
            }

            // This will be updated further when encountering multi character tokens.
            self.pos += 1;

            return newToken;
        }
    };
}
