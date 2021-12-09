const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 16813);
    const result = try run(allocator, input);
    try stdout.print("{}\n", .{result});
}

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !usize {
    var heightmap_list = std.ArrayList([]const u8).init(allocator);
    defer {
        for (heightmap_list.items) |row| {
            allocator.free(row);
        }
        heightmap_list.deinit();
    }

    var lines = std.mem.tokenize(input, "\n");
    while (lines.next()) |line| {
        var row = try std.ArrayList(u8).initCapacity(allocator, line.len);
        defer row.deinit();

        for (line) |char| {
            try row.append(char - '0');
        }

        try heightmap_list.append(row.toOwnedSlice());
    }

    const heightmap = heightmap_list.items;
    var risk_level: usize = 0;

    var y: usize = 0;
    while (y < heightmap.len) : (y += 1) {
        var x: usize = 0;
        while (x < heightmap[y].len) : (x += 1) {
            if (is_low_point(heightmap, x, y)) {
                risk_level += @as(usize, heightmap[y][x]) + 1;
            }
        }
    }

    return risk_level;
}

fn is_low_point(heightmap: []const []const u8, x: usize, y: usize) bool {
    const value = heightmap[y][x];

    if (x > 0 and heightmap[y][x - 1] <= value) {
        return false;
    } else if (x < heightmap[y].len - 1 and heightmap[y][x + 1] <= value) {
        return false;
    } else if (y > 0 and heightmap[y - 1][x] <= value) {
        return false;
    } else if (y < heightmap.len - 1 and heightmap[y + 1][x] <= value) {
        return false;
    } else {
        return true;
    }
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\2199943210
        \\3987894921
        \\9856789892
        \\8767896789
        \\9899965678
    );
    try expect(result == 15);
}
