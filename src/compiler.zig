const std = @import("std");

pub fn Compiler() type {
    return struct {
        const Self = @This();

        source: []u8,

        pub fn init(source: []u8) Self {
            return Self{
                .source = source,
            };
        }

        pub fn interpret(self: *Self) !void {
            _ = self;
            //
        }

        pub fn compile(self: *Self) !void {
            try self.interpret();
        }
    };
}
