const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 9268);
    const result = try run(allocator, input);
    try stdout.print("{}\n", .{result});
}

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !isize {
    var map = std.AutoHashMap(Point, usize).init(allocator);
    defer map.deinit();

    var lines = std.mem.tokenize(input, "\n");
    while (lines.next()) |line| {
        const entry = try parse_entry(line);
        try mark(&map, entry.from);

        const x_dir = calculate_dir(entry.from.x, entry.to.x);
        const y_dir = calculate_dir(entry.from.y, entry.to.y);
        var x: isize = @intCast(isize, entry.from.x) + x_dir;
        var y: isize = @intCast(isize, entry.from.y) + y_dir;
        const to_x: isize = @intCast(isize, entry.to.x);
        const to_y: isize = @intCast(isize, entry.to.y);

        while (x != to_x or y != to_y) : ({
            x += x_dir;
            y += y_dir;
        }) {
            try mark(&map, .{ .x = @intCast(usize, x), .y = @intCast(usize, y) });
        }

        try mark(&map, entry.to);
    }

    var sum: isize = 0;
    var counts = map.valueIterator();
    while (counts.next()) |count| {
        if (count.* >= 2) {
            sum += 1;
        }
    }
    return sum;
}

const Point = struct {
    x: usize,
    y: usize,
};

const Line = struct {
    from: Point,
    to: Point,
};

fn parse_entry(entry: []const u8) !Line {
    var it = std.mem.tokenize(entry, " ");
    const from = try parse_point(it.next().?);
    try expect(std.mem.eql(u8, it.next().?, "->"));
    const to = try parse_point(it.next().?);
    return Line{ .from = from, .to = to };
}

fn parse_point(s: []const u8) !Point {
    var it = std.mem.tokenize(s, ",");
    return Point{
        .x = try std.fmt.parseInt(usize, it.next().?, 10),
        .y = try std.fmt.parseInt(usize, it.next().?, 10),
    };
}

fn mark(map: *std.AutoHashMap(Point, usize), point: Point) !void {
    var entry = try map.getOrPut(point);
    if (!entry.found_existing) {
        entry.value_ptr.* = 0;
    }
    entry.value_ptr.* += 1;
}

fn calculate_dir(from: usize, to: usize) isize {
    if (to > from) {
        return 1;
    } else if (to < from) {
        return -1;
    } else {
        return 0;
    }
}

fn print_map(map: std.AutoHashMap(Point, usize), width: usize, height: usize) void {
    std.debug.print("\n", .{});
    var y: usize = 0;
    while (y < height) : (y += 1) {
        var x: usize = 0;
        while (x < width) : (x += 1) {
            if (map.get(Point{ .x = x, .y = y })) |count| {
                std.debug.print("{}", .{count});
            } else {
                std.debug.print(".", .{});
            }
        }

        std.debug.print("\n", .{});
    }
    std.debug.print("\n", .{});
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\0,9 -> 5,9
        \\8,0 -> 0,8
        \\9,4 -> 3,4
        \\2,2 -> 2,1
        \\7,0 -> 7,4
        \\6,4 -> 2,0
        \\0,9 -> 2,9
        \\3,4 -> 1,4
        \\0,0 -> 8,8
        \\5,5 -> 8,2
    );
    try expect(result == 12);
}
