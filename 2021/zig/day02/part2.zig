const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 7836);
    const result = try run(allocator, input);
    try stdout.print("{}\n", .{result});
}

const Position = struct {
    x: i32,
    y: i32,
    aim: i32,
};

const Direction = enum {
    up,
    down,
    forward,
};

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !i32 {
    var pos = Position{
        .x = 0,
        .y = 0,
        .aim = 0,
    };

    var lines = std.mem.tokenize(input, "\n");
    while (lines.next()) |line| {
        var words = std.mem.split(std.mem.trim(u8, line, " "), " ");
        const raw_direction = words.next().?;
        const raw_distance = words.next().?;

        const direction = std.meta.stringToEnum(Direction, raw_direction).?;
        const distance = try std.fmt.parseInt(i32, raw_distance, 10);

        switch (direction) {
            .up => pos.aim -= distance,
            .down => pos.aim += distance,
            .forward => {
                pos.x += distance;
                pos.y += (pos.aim * distance);
            },
        }
    }

    return pos.x * pos.y;
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\ forward 5
        \\ down 5
        \\ forward 8
        \\ up 3
        \\ down 8
        \\ forward 2
    );
    try expect(result == 900);
}
