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

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !isize {
    var measurements = std.ArrayList(isize).init(allocator);
    defer measurements.deinit();

    var it = std.mem.tokenize(input, " \n");
    while (it.next()) |value| {
        const n = try std.fmt.parseInt(isize, value, 10);
        try measurements.append(n);
    }

    comptime const window_size = 3;
    var increases: isize = 0;

    var i: usize = 0;
    while (i < measurements.items.len - window_size) : (i += 1) {
        const sum1 = sum(measurements.items[i..(i + window_size)]);
        const sum2 = sum(measurements.items[i + 1 .. (i + 1 + window_size)]);
        if (sum2 > sum1) {
            increases += 1;
        }
    }

    return increases;
}

fn sum(numbers: []const isize) isize {
    var result: isize = 0;
    for (numbers) |number| {
        result += number;
    }
    return result;
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
    try expect(result == 5);
}
