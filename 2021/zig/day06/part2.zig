const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 600);
    const result = try run(allocator, input);
    try stdout.print("{}\n", .{result});
}

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !usize {
    var fish_by_age = std.AutoHashMap(usize, usize).init(allocator);
    defer fish_by_age.deinit();

    var input_iter = std.mem.tokenize(input, ",");
    while (input_iter.next()) |raw_age| {
        const age = std.fmt.parseInt(usize, std.mem.trim(u8, raw_age, "\n"), 10) catch |err| {
            std.debug.print("not an int: '{s}'\n", .{raw_age});
            return err;
        };
        try put_or_add(usize, usize, &fish_by_age, age, 1);
    }

    comptime var day: isize = 0;
    inline while (day < 256) : (day += 1) {
        try run_day(&fish_by_age);
    }

    return total_fish(&fish_by_age);
}

fn total_fish(fish_by_age: *std.AutoHashMap(usize, usize)) usize {
    var sum: usize = 0;
    var fish_iter = fish_by_age.valueIterator();
    while (fish_iter.next()) |count| {
        sum += count.*;
    }
    return sum;
}

fn put_or_add(comptime K: type, comptime V: type, map: *std.AutoHashMap(K, V), key: K, value: V) !void {
    var entry = try map.getOrPut(key);
    if (!entry.found_existing) {
        entry.value_ptr.* = 0;
    }
    entry.value_ptr.* += value;
}

fn run_day(fish_by_age: *std.AutoHashMap(usize, usize)) !void {
    var new_fish_by_age = try fish_by_age.clone();
    new_fish_by_age.clearRetainingCapacity();
    defer new_fish_by_age.deinit();

    var fish_iter = fish_by_age.iterator();
    while (fish_iter.next()) |entry| {
        if (entry.key_ptr.* == 0) {
            try put_or_add(usize, usize, &new_fish_by_age, 6, entry.value_ptr.*);
            try put_or_add(usize, usize, &new_fish_by_age, 8, entry.value_ptr.*);
        } else {
            try put_or_add(usize, usize, &new_fish_by_age, entry.key_ptr.* - 1, entry.value_ptr.*);
        }
    }

    fish_by_age.clearRetainingCapacity();
    var new_iter = new_fish_by_age.iterator();
    while (new_iter.next()) |entry| {
        try fish_by_age.put(entry.key_ptr.*, entry.value_ptr.*);
    }
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\3,4,3,1,2
    );
    try expect(result == 26984457539);
}
