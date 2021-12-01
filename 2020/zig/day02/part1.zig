const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 20747);
    const result = try run(allocator, input);
    try stdout.print("{}\n", .{result});
}

const BasicPolicy = struct {
    letter: u8,
    low: i32,
    high: i32,

    fn is_valid(self: @This(), password: []const u8) bool {
        var count: i32 = 0;
        for (password) |letter| {
            if (self.letter == letter) {
                count += 1;
            }
        }
        return count >= self.low and count <= self.high;
    }

    fn parse(str: []const u8) !BasicPolicy {
        var halves = std.mem.tokenize(str, " ");
        var lowAndHigh = std.mem.tokenize(halves.next().?, "-");
        const policy = BasicPolicy{
            .low = try std.fmt.parseInt(i32, lowAndHigh.next().?, 10),
            .high = try std.fmt.parseInt(i32, lowAndHigh.next().?, 10),
            .letter = halves.next().?[0],
        };
        return policy;
    }
};

// unused, but this might be a neat pattern
fn PasswordWithPolicy(comptime T: type) type {
    return struct {
        policy: T,
        password: []const u8,

        fn is_valid(self: @This()) bool {
            return self.policy.is_valid(self.password);
        }
    };
}

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !i32 {
    var num_valid: i32 = 0;

    var it = std.mem.tokenize(input, "\n");
    while (it.next()) |line| {
        var parts = std.mem.tokenize(line, ":");
        const policy = try BasicPolicy.parse(std.mem.trim(u8, parts.next().?, " "));
        const password = parts.next().?;
        if (policy.is_valid(password)) {
            num_valid += 1;
        }
    }

    return num_valid;
}

test "parse basic policy" {
    const policy = try BasicPolicy.parse("1-3 a");
    // Is there a way to compare structs directly?
    try expect(policy.low == 1);
    try expect(policy.high == 3);
    try expect(policy.letter == 'a');
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\ 1-3 a: abcde
        \\ 1-3 b: cdefg
        \\ 2-9 c: ccccccccc
    );
    try expect(result == 2);
}
