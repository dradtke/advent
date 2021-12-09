// This solution works, but takes an ungodly amount of time to run.
// It can probably be optimized, but whatevs.

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

const Point = struct {
    x: usize,
    y: usize,

    fn left(self: @This(), heightmap: []const []const u8) ?Point {
        if (self.x == 0) {
            return null;
        } else {
            return Point{ .x = self.x - 1, .y = self.y };
        }
    }

    fn right(self: @This(), heightmap: []const []const u8) ?Point {
        if (self.x == heightmap[0].len - 1) {
            return null;
        } else {
            return Point{ .x = self.x + 1, .y = self.y };
        }
    }

    fn up(self: @This(), heightmap: []const []const u8) ?Point {
        if (self.y == 0) {
            return null;
        } else {
            return Point{ .x = self.x, .y = self.y - 1 };
        }
    }

    fn down(self: @This(), heightmap: []const []const u8) ?Point {
        if (self.y == heightmap.len - 1) {
            return null;
        } else {
            return Point{ .x = self.x, .y = self.y + 1 };
        }
    }
};

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
    var visited = std.AutoHashMap(Point, void).init(allocator);
    defer visited.deinit();

    var basin_sizes = std.ArrayList(usize).init(allocator);
    defer basin_sizes.deinit();

    var y: usize = 0;
    while (y < heightmap.len) : (y += 1) {
        var x: usize = 0;
        while (x < heightmap[y].len) : (x += 1) {
            const point = Point{ .x = x, .y = y };
            if (is_low_point(heightmap, point)) {
                const basin_size = try calculate_basin_size(heightmap, &visited, point);
                try basin_sizes.append(basin_size);
            }
        }
    }

    std.sort.sort(usize, basin_sizes.items, {}, comptime std.sort.desc(usize));
    return basin_sizes.items[0] * basin_sizes.items[1] * basin_sizes.items[2];
}

fn can_reach(heightmap: []const []const u8, visited: *std.AutoHashMap(Point, void), from: Point, to: Point) error{OutOfMemory}!bool {
    if (from.x == to.x and from.y == to.y) {
        return true;
    }
    if (heightmap[from.y][from.x] == 9) {
        return false;
    }
    try visited.put(from, {});
    if (from.left(heightmap)) |new_from| {
        if (!visited.contains(new_from) and try can_reach(heightmap, visited, new_from, to)) {
            return true;
        }
    }
    if (from.right(heightmap)) |new_from| {
        if (!visited.contains(new_from) and try can_reach(heightmap, visited, new_from, to)) {
            return true;
        }
    }
    if (from.up(heightmap)) |new_from| {
        if (!visited.contains(new_from) and try can_reach(heightmap, visited, new_from, to)) {
            return true;
        }
    }
    if (from.down(heightmap)) |new_from| {
        if (!visited.contains(new_from) and try can_reach(heightmap, visited, new_from, to)) {
            return true;
        }
    }
    return false;
}

fn calculate_basin_size(heightmap: []const []const u8, visited: *std.AutoHashMap(Point, void), low_point: Point) !usize {
    var size: usize = 0;
    var y: usize = 0;
    while (y < heightmap.len) : (y += 1) {
        var x: usize = 0;
        while (x < heightmap[y].len) : (x += 1) {
            visited.clearRetainingCapacity();
            if (try can_reach(heightmap, visited, Point{ .x = x, .y = y }, low_point)) {
                size += 1;
            }
        }
    }
    return size;
}

fn is_low_point(heightmap: []const []const u8, point: Point) bool {
    const value = heightmap[point.y][point.x];

    if (point.x > 0 and heightmap[point.y][point.x - 1] <= value) {
        return false;
    } else if (point.x < heightmap[point.y].len - 1 and heightmap[point.y][point.x + 1] <= value) {
        return false;
    } else if (point.y > 0 and heightmap[point.y - 1][point.x] <= value) {
        return false;
    } else if (point.y < heightmap.len - 1 and heightmap[point.y + 1][point.x] <= value) {
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
    try expect(result == 1134);
}
