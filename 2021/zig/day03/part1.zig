const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 13000);
    const result = try run(allocator, input);
    try stdout.print("{}\n", .{result});
}

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !isize {
    var numbers = std.ArrayList([]const u8).init(allocator);
    defer numbers.deinit();

    var lines = std.mem.tokenize(input, " \n");
    while (lines.next()) |line| {
        try numbers.append(line);
    }

    const line_length = numbers.items[0].len;
    var gamma_bits = try std.ArrayList(u8).initCapacity(allocator, line_length);
    defer gamma_bits.deinit();
    var epsilon_bits = try std.ArrayList(u8).initCapacity(allocator, line_length);
    defer epsilon_bits.deinit();

    var i: usize = 0;
    while (i < line_length) : (i += 1) {
        var counts = try count_bits(allocator, numbers.items, i);
        try gamma_bits.append(key_for_value(counts, .highest));
        try epsilon_bits.append(key_for_value(counts, .lowest));
        counts.deinit();
    }

    const gamma = try std.fmt.parseInt(isize, gamma_bits.items, 2);
    const epsilon = try std.fmt.parseInt(isize, epsilon_bits.items, 2);

    return gamma * epsilon;
}

const BitCounts = std.AutoHashMap(u8, usize);

fn count_bits(allocator: *std.mem.Allocator, numbers: []const []const u8, position: usize) !BitCounts {
    var counts = BitCounts.init(allocator);
    errdefer counts.deinit();
    for (numbers) |number| {
        const bit = number[position];
        var entry = try counts.getOrPut(bit);
        if (!entry.found_existing) {
            entry.value_ptr.* = 0;
        }
        entry.value_ptr.* += 1;
    }
    return counts;
}

const KeyForValueOperation = enum {
    highest,
    lowest,

    fn compare(self: @This(), old: anytype, new: anytype) bool {
        return switch (self) {
            .highest => new > old,
            .lowest => new < old,
        };
    }
};

fn key_for_value(counts: BitCounts, op: KeyForValueOperation) u8 {
    var it = counts.iterator();
    var first = it.next().?;

    var result_entry = first.key_ptr.*;
    var result_value = first.value_ptr.*;

    while (it.next()) |entry| {
        if (op.compare(result_value, entry.value_ptr.*)) {
            result_entry = entry.key_ptr.*;
        }
    }

    return result_entry;
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\ 00100
        \\ 11110
        \\ 10110
        \\ 10111
        \\ 10101
        \\ 01111
        \\ 00111
        \\ 11100
        \\ 10000
        \\ 11001
        \\ 00010
        \\ 01010
    );
    try expect(result == 198);
}
