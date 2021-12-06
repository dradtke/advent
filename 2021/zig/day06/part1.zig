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
    var fish_list = std.ArrayList(Fish).init(allocator);
    defer fish_list.deinit();

    var new_fish_list = std.ArrayList(Fish).init(allocator);
    defer new_fish_list.deinit();

    var input_iter = std.mem.tokenize(input, ",");
    while (input_iter.next()) |raw_age| {
        const age = std.fmt.parseInt(isize, std.mem.trim(u8, raw_age, "\n"), 10) catch |err| {
            std.debug.print("not an int: '{s}'\n", .{raw_age});
            return err;
        };
        try fish_list.append(Fish{ .age = age });
    }

    comptime var day: isize = 0;
    inline while (day < 80) : (day += 1) {
        try run_day(&fish_list, &new_fish_list);
    }

    return fish_list.items.len;
}

const Fish = struct {
    age: isize,
};

fn run_day(fish_list: *std.ArrayList(Fish), new_fish_list: *std.ArrayList(Fish)) !void {
    new_fish_list.clearRetainingCapacity();
    for (fish_list.items) |*fish| {
        if (fish.age == 0) {
            fish.*.age = 6;
            try new_fish_list.append(Fish{ .age = 8 });
        } else {
            fish.*.age -= 1;
        }
    }
    try fish_list.appendSlice(new_fish_list.items);
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\3,4,3,1,2
    );
    try expect(result == 5934);
}
