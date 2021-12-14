const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 16813);
    try run(allocator, input);
}

const Point = struct {
    x: isize,
    y: isize,
};

const Paper = std.AutoHashMap(Point, void);

const Fold = union(enum) {
    x: isize,
    y: isize,
};

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !void {
    var paper = Paper.init(allocator);
    defer paper.deinit();

    var x_max: isize = 0;
    var y_max: isize = 0;

    var lines = std.mem.split(input, "\n");
    // grab the dots
    while (lines.next()) |line| {
        if (line.len == 0) break;
        var numbers = std.mem.tokenize(line, ",");
        const point = Point{
            .x = try std.fmt.parseInt(isize, numbers.next().?, 10),
            .y = try std.fmt.parseInt(isize, numbers.next().?, 10),
        };
        try paper.put(point, {});
        if (point.x > x_max) x_max = point.x;
        if (point.y > y_max) y_max = point.y;
    }

    while (lines.next()) |line| {
        if (line.len == 0) break;
        const fold = try parse_fold_instruction(line);
        try apply_fold(allocator, &paper, fold);
        switch (fold) {
            Fold.x => |n| x_max = n,
            Fold.y => |n| y_max = n,
        }
    }

    var y: isize = 0;
    while (y < y_max) : (y += 1) {
        var x: isize = 0;
        while (x < x_max) : (x += 1) {
            if (paper.contains(Point{ .x = x, .y = y })) {
                std.debug.print("#", .{});
            } else {
                std.debug.print(".", .{});
            }
        }
        std.debug.print("\n", .{});
    }
}

fn parse_fold_instruction(line: []const u8) !Fold {
    const prefix = "fold along ";
    if (!std.mem.eql(u8, line[0..prefix.len], prefix)) {
        return error.InvalidFoldInstruction;
    }
    var spec = std.mem.tokenize(line[prefix.len..], "=");
    const axis = spec.next().?;
    const n = try std.fmt.parseInt(isize, spec.next().?, 10);
    return switch (axis[0]) {
        'x' => Fold{ .x = n },
        'y' => Fold{ .y = n },
        else => error.InvalidFoldAxis,
    };
}

fn apply_fold(allocator: *std.mem.Allocator, paper: *Paper, fold: Fold) !void {
    var new_paper = Paper.init(allocator);
    defer new_paper.deinit();

    switch (fold) {
        Fold.x => |n| {
            var iter = paper.iterator();
            while (iter.next()) |entry| {
                const x = entry.key_ptr.*.x;
                const point = Point{
                    .x = if (x < n) x else n - (x - n),
                    .y = entry.key_ptr.*.y,
                };
                try new_paper.put(point, {});
            }
        },
        Fold.y => |n| {
            var iter = paper.iterator();
            while (iter.next()) |entry| {
                const y = entry.key_ptr.*.y;
                const point = Point{
                    .x = entry.key_ptr.*.x,
                    .y = if (y < n) y else n - (y - n),
                };
                try new_paper.put(point, {});
            }
        },
    }

    paper.clearRetainingCapacity();
    var iter = new_paper.iterator();
    while (iter.next()) |entry| {
        try paper.put(entry.key_ptr.*, entry.value_ptr.*);
    }
}

test "example scenario" {
    try run(std.testing.allocator,
        \\6,10
        \\0,14
        \\9,10
        \\0,3
        \\10,4
        \\4,11
        \\6,0
        \\6,12
        \\4,1
        \\0,13
        \\10,12
        \\3,4
        \\3,0
        \\8,4
        \\1,10
        \\2,14
        \\8,10
        \\9,0
        \\
        \\fold along y=7
        \\fold along x=5
    );
}
