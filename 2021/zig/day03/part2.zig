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

    const oxygen_generator_rating_bits = try calculate_rating(allocator, numbers.items, std.sort.desc, '1');
    const co2_scrubber_rating_bits = try calculate_rating(allocator, numbers.items, std.sort.asc, '0');

    const oxygen_generator_rating = try std.fmt.parseInt(isize, oxygen_generator_rating_bits, 2);
    const co2_scrubber_rating = try std.fmt.parseInt(isize, co2_scrubber_rating_bits, 2);

    return oxygen_generator_rating * co2_scrubber_rating;
}

const BitCounts = std.AutoHashMap(u8, usize);

fn count_bits(allocator: *std.mem.Allocator, numbers: std.StringHashMap(void), position: usize) !BitCounts {
    var counts = BitCounts.init(allocator);
    errdefer counts.deinit();

    var iter = numbers.keyIterator();
    while (iter.next()) |number| {
        const bit = number.*[position];
        var entry = try counts.getOrPut(bit);
        if (!entry.found_existing) {
            entry.value_ptr.* = 0;
        }
        entry.value_ptr.* += 1;
    }
    return counts;
}

fn calculate_rating(allocator: *std.mem.Allocator, numbers: []const []const u8, comparator: anytype, default: u8) ![]const u8 {
    var number_set = std.StringHashMap(void).init(allocator);
    defer number_set.deinit();
    for (numbers) |number| {
        try number_set.put(number, {});
    }

    const line_length = numbers[0].len;
    var i: usize = 0;
    while (i < line_length) : (i += 1) {
        var counts = try count_bits(allocator, number_set, i);
        const important_bit = (try key_for_value(allocator, counts, comparator)) orelse default;
        counts.deinit();
        // This would be more efficient if we could only iterate through existing items in the set.
        for (numbers) |number| {
            if (number[i] != important_bit) {
                _ = number_set.remove(number);
            }
        }
        if (number_set.count() == 1) {
            return number_set.keyIterator().next().?.*;
        }
    }
    unreachable;
}

fn key_for_value(allocator: *std.mem.Allocator, counts: BitCounts, comparator: anytype) !?u8 {
    const BitCountEntry = struct {
        bit: u8,
        count: usize,

        fn compare(context: void, lhs: @This(), rhs: @This()) bool {
            return comparator(usize)({}, lhs.count, rhs.count);
        }
    };
    var list = std.ArrayList(BitCountEntry).init(allocator);
    defer list.deinit();

    var it = counts.iterator();
    while (it.next()) |entry| {
        // TODO: This apparently results in a memory leak, but I'm not sure how...
        try list.append(.{ .bit = entry.key_ptr.*, .count = entry.value_ptr.* });
    }

    var count_items = list.toOwnedSlice();
    std.sort.sort(BitCountEntry, count_items, {}, comptime BitCountEntry.compare);

    if (count_items[0].count == count_items[1].count) {
        return null;
    } else {
        return count_items[0].bit;
    }
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
    try expect(result == 230);
}
