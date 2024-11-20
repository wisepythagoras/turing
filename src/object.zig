const std = @import("std");
const func = @import("function.zig");

pub const ObjectType = enum(u8) {
    STRING,
    FUNCTION,

    pub fn fromU8(byte: u8) !ObjectType {
        return std.meta.intToEnum(@This(), byte);
    }
};

pub const ObjectValueUnion = union {
    string: String(),
    func: func.Function(),
};

pub fn Object() type {
    return struct {
        const Self = @This();

        objType: ObjectType,
        val: ObjectValueUnion,

        pub fn init(val: anytype) ?Self {
            if (@TypeOf(val) == String()) {
                return Self{
                    .objType = ObjectType.STRING,
                    .val = ObjectValueUnion{
                        .string = val,
                    },
                };
            } else if (@TypeOf(val) == func.Function()) {
                return Self{
                    .objType = ObjectType.FUNCTION,
                    .val = ObjectValueUnion{
                        .func = val,
                    },
                };
            }

            return null;
        }

        /// Checks two objects for equality.
        pub fn isEqual(self: Self, obj: *Object()) bool {
            if (self.objType != obj.objType) {
                return false;
            }

            if (self.objType == ObjectType.STRING) {
                return self.val.string.isEqual(&obj.val.string);
            } else if (self.objType == ObjectType.FUNCTION) {
                return self.val.func.isEqual(&obj.val.func);
            }

            return false;
        }

        /// Returns the bytes that comprise this object.
        pub fn toBytes(self: Self) ![]const u8 {
            const memory = std.heap.c_allocator;

            if (self.objType == ObjectType.STRING) {
                const buf = try memory.alloc(u8, self.val.string.len + 1);
                buf[0] = @as(u8, @intFromEnum(self.objType));

                const str = self.toString();
                var i: usize = 1;

                for (str) |char| {
                    buf[i] = char;
                    i += 1;
                }

                return buf;
            } else if (self.objType == ObjectType.FUNCTION) {
                // TODO: Implement.
            }

            return "";
        }

        /// Converts the value object to a string value.
        pub fn toString(self: Self) []const u8 {
            if (self.objType == ObjectType.STRING) {
                return self.val.string.chars;
            } else if (self.objType == ObjectType.FUNCTION) {
                return "function";
            }

            return "";
        }
    };
}

pub fn String() type {
    return struct {
        const Self = @This();

        len: usize,
        chars: []const u8,

        pub fn init(str: []const u8) Self {
            return Self{
                .chars = str,
                .len = str.len,
            };
        }

        pub fn isEqual(self: Self, obj: *String()) bool {
            return std.mem.eql(u8, obj.chars, self.chars);
        }
    };
}
