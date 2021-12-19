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

const Action = struct {
    letter: u8,
    position: usize,
};

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !usize {
    var lines = std.mem.tokenize(input, "\n");
    var rules = std.StringHashMap(u8).init(allocator);
    defer rules.deinit();

    const template = lines.next().?;
    while (lines.next()) |line| {
        if (line.len != 7 or !std.mem.eql(u8, line[2..6], " -> ")) {
            return error.InvalidRuleFormat;
        }
        try rules.put(line[0..2], line[6]);
    }

    var polymer = try std.ArrayList(u8).initCapacity(allocator, template.len);
    defer polymer.deinit();
    for (template) |char| {
        try polymer.append(char);
    }

    var actions = std.ArrayList(Action).init(allocator);
    defer actions.deinit();

    comptime var n = 0;
    inline while (n < 10) : (n += 1) {
        actions.clearRetainingCapacity();
        try step(&actions, &polymer, rules);
    }

    var counts = try get_counts(allocator, polymer.items);
    defer counts.deinit();

    var least: usize = 0;
    var most: usize = 0;

    var values = counts.valueIterator();
    while (values.next()) |value| {
        if (least == 0 or value.* < least) least = value.*;
        if (most == 0 or value.* > most) most = value.*;
    }

    return most - least;
}

fn step(actions: *std.ArrayList(Action), polymer: *std.ArrayList(u8), rules: std.StringHashMap(u8)) !void {
    var i: usize = 0;
    while (i < polymer.items.len - 1) : (i += 1) {
        const pair = polymer.items[i .. i + 2];
        if (rules.get(pair)) |rule| {
            try actions.append(Action{ .letter = rule, .position = i + 1 });
        }
    }
    i = 0;
    while (i < actions.items.len) : (i += 1) {
        try polymer.insert(actions.items[i].position + i, actions.items[i].letter);
    }
}

fn get_counts(allocator: *std.mem.Allocator, polymer: []const u8) !std.AutoHashMap(u8, usize) {
    var result = std.AutoHashMap(u8, usize).init(allocator);
    errdefer result.deinit();
    for (polymer) |char| {
        var entry = try result.getOrPut(char);
        if (!entry.found_existing) {
            entry.value_ptr.* = 0;
        }
        entry.value_ptr.* += 1;
    }
    return result;
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\NNCB
        \\
        \\CH -> B
        \\HH -> N
        \\CB -> H
        \\NH -> C
        \\HB -> C
        \\HC -> B
        \\HN -> C
        \\NN -> C
        \\BH -> H
        \\NC -> B
        \\NB -> B
        \\BN -> B
        \\BB -> N
        \\BC -> B
        \\CC -> N
        \\CN -> C
    );
    try expect(result == 1588);
}
