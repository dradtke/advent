const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 9886);
    const result = try run(allocator, input);
    try stdout.print("{}\n", .{result});
}

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !i32 {
    var measurements = std.ArrayList(i32).init(allocator);
    defer measurements.deinit();

    var it = std.mem.tokenize(input, " \n");
    while (it.next()) |value| {
        const n = try std.fmt.parseInt(i32, value, 10);
        try measurements.append(n);
    }

    var increases: i32 = 0;

    var i: usize = 0;
    while (i < measurements.items.len - 1) : (i += 1) {
        if (measurements.items[i] < measurements.items[i + 1]) {
            increases += 1;
        }
    }

    return increases;
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\ 199
        \\ 200
        \\ 208
        \\ 210
        \\ 200
        \\ 207
        \\ 240
        \\ 269
        \\ 260
        \\ 263
    );
    try expect(result == 7);
}
