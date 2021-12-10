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

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !isize {
    var lines = std.mem.tokenize(input, "\n");
    var chars = std.ArrayList(u8).init(allocator);
    defer chars.deinit();

    var scores = std.ArrayList(isize).init(allocator);
    defer scores.deinit();

    while (lines.next()) |line| {
        var score: isize = 0;
        chars.clearRetainingCapacity();
        find_opening_characters(&chars, line) catch |err| switch (err) {
            error.LineCorrupted => continue,
            else => return err,
        };
        while (chars.popOrNull()) |char| {
            score *= 5;
            score += switch (char) {
                '(' => @as(isize, 1),
                '[' => @as(isize, 2),
                '{' => @as(isize, 3),
                '<' => @as(isize, 4),
                else => unreachable,
            };
        }
        try scores.append(score);
    }
    std.sort.sort(isize, scores.items, {}, comptime std.sort.asc(isize));
    return scores.items[(scores.items.len - 1) / 2];
}

fn find_opening_characters(chars: *std.ArrayList(u8), line: []const u8) error{ OutOfMemory, LineCorrupted }!void {
    for (line) |char| {
        switch (char) {
            '(', '[', '{', '<' => try chars.append(char),
            ')' => if (chars.pop() != '(') return error.LineCorrupted,
            ']' => if (chars.pop() != '[') return error.LineCorrupted,
            '}' => if (chars.pop() != '{') return error.LineCorrupted,
            '>' => if (chars.pop() != '<') return error.LineCorrupted,
            else => unreachable,
        }
    }
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\[({(<(())[]>[[{[]{<()<>>
        \\[(()[<>])]({[<{<<[]>>(
        \\{([(<{}[<>[]}>{[]{[(<()>
        \\(((({<>}<{<{<>}{[]{[]{}
        \\[[<[([]))<([[{}[[()]]]
        \\[{[{({}]{}}([{[{{{}}([]
        \\{<[[]]>}<{[{[{[]{()[[[]
        \\[<(<(<(<{}))><([]([]()
        \\<{([([[(<>()){}]>(<<{{
        \\<{([{{}}[<[[[<>{}]]]>[]]
    );
    try expect(result == 288957);
}
