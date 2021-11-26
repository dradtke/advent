const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

// fn readLine(r: anytype, allocator: *std.mem.Allocator) !?[]u8 {
//     return try r.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024);
// }
//
// fn readAllLines(r: anytype, allocator: *std.mem.Allocator) !std.ArrayList([]u8) {
//     var lines = std.ArrayList([]u8).init(allocator);
//     while (try readLine(r, allocator)) |line| {
//         try lines.append(line);
//     }
//     return lines;
// }
//
// fn items(r: anytype, allocator: *std.mem.Allocator) !std.mem.TokenIterator {
//     std.mem.tokenize(data, " \n");
// }

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 4096);
    const result = try run(allocator, input);
    try stdout.print("{}\n", .{result});
}

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !i32 {
    var numbers = std.ArrayList(i32).init(allocator);
    defer numbers.deinit();

    var it = std.mem.tokenize(input, " \n");
    while (it.next()) |value| {
        const n = try std.fmt.parseInt(i32, value, 10);
        try numbers.append(n);
    }
    for (numbers.items) |x, i| {
        for (numbers.items) |y, j| {
            if (i == j) {
                continue;
            } else if (x + y == 2020) {
                return x * y;
            }
        }
    }
    unreachable;
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\ 1721
        \\ 979
        \\ 366
        \\ 299
        \\ 675
        \\ 1456
    );
    try expect(result == 514579);
}
