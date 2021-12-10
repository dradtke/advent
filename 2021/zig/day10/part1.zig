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
    var score: isize = 0;
    var lines = std.mem.tokenize(input, "\n");
    while (lines.next()) |line| {
        if (try find_first_illegal_character(allocator, line)) |char| {
            switch (char) {
                ')' => score += 3,
                ']' => score += 57,
                '}' => score += 1197,
                '>' => score += 25137,
                else => unreachable,
            }
        }
    }
    return score;
}

fn find_first_illegal_character(allocator: *std.mem.Allocator, line: []const u8) !?u8 {
    var chars = std.ArrayList(u8).init(allocator);
    defer chars.deinit();
    for (line) |char| {
        switch (char) {
            '(', '[', '{', '<' => try chars.append(char),
            ')' => if (chars.pop() != '(') return char,
            ']' => if (chars.pop() != '[') return char,
            '}' => if (chars.pop() != '{') return char,
            '>' => if (chars.pop() != '<') return char,
            else => unreachable,
        }
    }
    return null;
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
    try expect(result == 26397);
}
