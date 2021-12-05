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
        if (entry.from.x == entry.to.x) {
            var y: usize = std.math.min(entry.from.y, entry.to.y);
            while (y <= std.math.max(entry.from.y, entry.to.y)) : (y += 1) {
                try mark(&map, .{ .x = entry.from.x, .y = y });
            }
        } else if (entry.from.y == entry.to.y) {
            var x: usize = std.math.min(entry.from.x, entry.to.x);
            while (x <= std.math.max(entry.from.x, entry.to.x)) : (x += 1) {
                try mark(&map, .{ .x = x, .y = entry.from.y });
            }
        } else {
            // ignore non-straight lines
        }
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
    try expect(result == 5);
}
