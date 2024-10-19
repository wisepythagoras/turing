pub const ObjectType = enum(u8) {
    STRING,
};

pub fn Object() type {
    return struct {
        const Self = @This();

        objType: ObjectType,

        pub fn init(t: ObjectType) Self {
            return Self{
                .objType = t,
            };
        }

        pub fn asString(self: *Self) *String() {
            return @as(*String(), @ptrCast(self));
        }
    };
}

pub fn String() type {
    return packed struct {
        const Self = @This();

        obj: Object(),
        chars: []const u8,
        len: usize,

        pub fn init(str: []const u8) Self {
            return Self{
                .obj = Object().init(ObjectType.STRING),
                .chars = str,
                .len = str.len,
            };
        }

        pub fn asObject(self: *Self) *Object() {
            return @as(*Object(), @ptrCast(self));
        }
    };
}
