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

const Pair = struct {
    a: u8,
    b: u8,

    fn str(self: @This()) []const u8 {
        var s = [2]u8{ 0, 0 };
        s[0] = self.a;
        s[1] = self.b;
        return &s;
    }
};

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !isize {
    var lines = std.mem.tokenize(input, "\n");
    var rules = std.AutoHashMap(Pair, u8).init(allocator);
    defer rules.deinit();

    const template = lines.next().?;
    while (lines.next()) |line| {
        if (line.len != 7 or !std.mem.eql(u8, line[2..6], " -> ")) {
            return error.InvalidRuleFormat;
        }
        const pair = Pair{ .a = line[0], .b = line[1] };
        try rules.put(pair, line[6]);
    }

    var pair_counts = std.AutoHashMap(Pair, isize).init(allocator);
    defer pair_counts.deinit();
    var letter_counts = std.AutoHashMap(u8, isize).init(allocator);
    defer letter_counts.deinit();

    var i: usize = 0;
    while (i < template.len) : (i += 1) {
        try map_add(u8, &letter_counts, template[i], 1);
        if (i < template.len - 1) {
            try map_add(Pair, &pair_counts, Pair{ .a = template[i], .b = template[i + 1] }, 1);
        }
    }

    comptime var n = 1;
    inline while (n <= 40) : (n += 1) {
        try step(&pair_counts, &letter_counts, rules);
    }

    var least: isize = 0;
    var most: isize = 0;

    var values = letter_counts.valueIterator();
    while (values.next()) |value| {
        if (least == 0 or value.* < least) least = value.*;
        if (most == 0 or value.* > most) most = value.*;
    }

    return most - least;
}

fn polymer_length(pair_counts: *std.AutoHashMap(Pair, isize)) isize {
    var length: isize = 1;
    var iter = pair_counts.iterator();
    while (iter.next()) |entry| {
        length += entry.value_ptr.*;
    }
    return length;
}

fn map_add(comptime T: type, map: *std.AutoHashMap(T, isize), key: T, value: isize) !void {
    var entry = try map.getOrPut(key);
    if (!entry.found_existing) {
        entry.value_ptr.* = 0;
    }
    entry.value_ptr.* += value;
    if (entry.value_ptr.* == 0) {
        _ = map.remove(key);
    }
}

fn step(pair_counts: *std.AutoHashMap(Pair, isize), letter_counts: *std.AutoHashMap(u8, isize), rules: std.AutoHashMap(Pair, u8)) !void {
    var snapshot = try pair_counts.clone();
    defer snapshot.deinit();
    var iter = snapshot.iterator();
    while (iter.next()) |entry| {
        const pair = entry.key_ptr.*;
        const n = entry.value_ptr.*;
        if (rules.get(pair)) |middle| {
            try map_add(u8, letter_counts, middle, n);
            try map_add(Pair, pair_counts, pair, -n);
            try map_add(Pair, pair_counts, Pair{ .a = pair.a, .b = middle }, n);
            try map_add(Pair, pair_counts, Pair{ .a = middle, .b = pair.b }, n);
        }
    }
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
    try expect(result == 2188189693529);
}
