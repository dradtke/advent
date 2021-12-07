const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 3903);
    const result = try run(allocator, input);
    try stdout.print("{}\n", .{result});
}

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !isize {
    var crab_positions = std.ArrayList(isize).init(allocator);
    defer crab_positions.deinit();

    var input_iter = std.mem.tokenize(input, ",");
    while (input_iter.next()) |raw_position| {
        const position = std.fmt.parseInt(isize, std.mem.trim(u8, raw_position, "\n"), 10) catch |err| {
            std.debug.print("not an int: '{s}'\n", .{raw_position});
            return err;
        };
        try crab_positions.append(position);
    }

    const min = std.sort.min(isize, crab_positions.items, {}, comptime std.sort.asc(isize)).?;
    const max = std.sort.max(isize, crab_positions.items, {}, comptime std.sort.asc(isize)).?;

    var result: ?isize = null;
    var dest: isize = min;
    while (dest <= max) : (dest += 1) {
        const fuel = try calculate_fuel(crab_positions, dest);
        if (result == null or fuel < result.?) {
            result = fuel;
        }
    }
    return result.?;
}

fn calculate_fuel(crab_positions: std.ArrayList(isize), dest: isize) !isize {
    var total: isize = 0;
    for (crab_positions.items) |position| {
        total += try std.math.absInt(dest - position);
    }
    return total;
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\16,1,2,0,4,2,7,1,2,14
    );
    try expect(result == 37);
}
