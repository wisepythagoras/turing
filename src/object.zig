const std = @import("std");

pub const ObjectType = enum(u8) {
    STRING,
};

pub const ObjectValueUnion = union {
    string: String(),
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
            }

            return false;
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
