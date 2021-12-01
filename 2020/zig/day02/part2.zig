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

const Policy = struct {
    letter: u8,
    index1: usize,
    index2: usize,

    fn is_valid(self: @This(), password: []const u8) bool {
        const isFirst = password[self.index1 - 1] == self.letter;
        const isSecond = password[self.index2 - 1] == self.letter;
        return (isFirst and !isSecond) or (!isFirst and isSecond);
    }

    fn parse(str: []const u8) !@This() {
        var halves = std.mem.tokenize(str, " ");
        var lowAndHigh = std.mem.tokenize(halves.next().?, "-");
        return Policy{
            .index1 = try std.fmt.parseInt(usize, lowAndHigh.next().?, 10),
            .index2 = try std.fmt.parseInt(usize, lowAndHigh.next().?, 10),
            .letter = halves.next().?[0],
        };
    }
};

// Like std.mem.TokenIterator, but each item has spaces trimmed.
const WordIterator = struct {
    it: std.mem.TokenIterator,

    fn init(line: []const u8, comptime delims: []const u8) @This() {
        return @This(){ .it = std.mem.tokenize(line, delims) };
    }

    fn next(self: *@This()) ?[]const u8 {
        if (self.it.next()) |item| {
            return std.mem.trim(u8, item, " ");
        } else {
            return null;
        }
    }
};

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !i32 {
    var num_valid: i32 = 0;

    var lines = std.mem.tokenize(input, "\n");
    while (lines.next()) |line| {
        var words = WordIterator.init(line, ":");
        const policy = try Policy.parse(words.next().?);
        const password = words.next().?;
        if (policy.is_valid(password)) {
            num_valid += 1;
        }
    }

    return num_valid;
}

test "parse policy" {
    const policy = try Policy.parse("1-3 a");
    // Is there a way to compare structs directly?
    try expect(policy.index1 == 1);
    try expect(policy.index2 == 3);
    try expect(policy.letter == 'a');
}

test "valid policy" {
    try expect((try Policy.parse("1-3 a")).is_valid("abcde") == true);
    try expect((try Policy.parse("1-3 b")).is_valid("cdefg") == false);
    try expect((try Policy.parse("2-9 c")).is_valid("ccccccccc") == false);
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\ 1-3 a: abcde
        \\ 1-3 b: cdefg
        \\ 2-9 c: ccccccccc
    );
    try expect(result == 1);
}
