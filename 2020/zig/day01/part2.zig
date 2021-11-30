const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

const Triple = struct { x: i32, y: i32, z: i32 };

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 4096);
    const result = try run(allocator, input);
    try stdout.print("{}\n", .{result});
}

fn run(allocator: *std.mem.Allocator, input: []const u8) !i32 {
    var numbers = std.ArrayList(i32).init(allocator);
    defer numbers.deinit();

    var it = std.mem.tokenize(input, " \n");
    while (it.next()) |value| {
        const n = try std.fmt.parseInt(i32, value, 10);
        try numbers.append(n);
    }

    var triples = try permutations(allocator, numbers.items);
    defer triples.deinit();
    for (triples.items) |triple| {
        if (triple.x + triple.y + triple.z == 2020) {
            return triple.x * triple.y * triple.z;
        }
    }

    unreachable;
}

fn permutations(allocator: *std.mem.Allocator, items: []i32) !std.ArrayList(Triple) {
    var result = std.ArrayList(Triple).init(allocator);
    errdefer result.deinit();

    for (items) |x, index1| {
        for (items) |y, index2| {
            for (items) |z, index3| {
                if (index1 == index2 or index1 == index3 or index2 == index3)
                    continue;
                try result.append(Triple{ .x = x, .y = y, .z = z });
            }
        }
    }
    return result;
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
    try expect(result == 241861950);
}
