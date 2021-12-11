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

const width = 10;
const height = 10;

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !isize {
    var octopuses = [_][width]u8{[_]u8{0} ** width} ** height;

    var lines = std.mem.tokenize(input, "\n");

    var y: isize = 0;
    while (y < octopuses.len) : (y += 1) {
        var line = lines.next().?;
        var x: isize = 0;
        while (x < octopuses[@intCast(usize, y)].len) : (x += 1) {
            octopuses[@intCast(usize, y)][@intCast(usize, x)] = line[@intCast(usize, x)] - '0';
        }
    }

    var i: isize = 0;
    while (true) {
        const flashes = step(&octopuses);
        if (flashes == width * height) {
            return i + 1;
        }
        i += 1;
    }

    unreachable;
}

fn step(octopuses: *[height][width]u8) isize {
    var step_flashes: isize = 0;

    var y: isize = 0;
    while (y < octopuses.len) : (y += 1) {
        var x: isize = 0;
        while (x < octopuses[@intCast(usize, y)].len) : (x += 1) {
            octopuses[@intCast(usize, y)][@intCast(usize, x)] += 1;
        }
    }

    while (true) {
        var any_flashed = false;

        y = 0;
        while (y < octopuses.len) : (y += 1) {
            var x: isize = 0;
            while (x < octopuses[@intCast(usize, y)].len) : (x += 1) {
                if (octopuses[@intCast(usize, y)][@intCast(usize, x)] > 9) {
                    flash(octopuses, x, y);
                    any_flashed = true;
                    step_flashes += 1;
                }
            }
        }

        if (!any_flashed) {
            break;
        }
    }

    return step_flashes;
}

fn flash(octopuses: *[height][width]u8, x: isize, y: isize) void {
    octopuses[@intCast(usize, y)][@intCast(usize, x)] = 0;
    if (grid_get(octopuses, x - 1, y)) |octopus| increase_energy(octopus);
    if (grid_get(octopuses, x - 1, y - 1)) |octopus| increase_energy(octopus);
    if (grid_get(octopuses, x, y - 1)) |octopus| increase_energy(octopus);
    if (grid_get(octopuses, x + 1, y - 1)) |octopus| increase_energy(octopus);
    if (grid_get(octopuses, x + 1, y)) |octopus| increase_energy(octopus);
    if (grid_get(octopuses, x + 1, y + 1)) |octopus| increase_energy(octopus);
    if (grid_get(octopuses, x, y + 1)) |octopus| increase_energy(octopus);
    if (grid_get(octopuses, x - 1, y + 1)) |octopus| increase_energy(octopus);
}

fn grid_get(grid: *[height][width]u8, x: isize, y: isize) ?*u8 {
    if (x < 0 or y < 0 or x > grid[0].len - 1 or y > grid.len - 1) {
        return null;
    } else {
        return &grid[@intCast(usize, y)][@intCast(usize, x)];
    }
}

fn increase_energy(octopus: *u8) void {
    if (octopus.* != 0) {
        octopus.* += 1;
    }
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\5483143223
        \\2745854711
        \\5264556173
        \\6141336146
        \\6357385478
        \\4167524645
        \\2176841721
        \\6882881134
        \\4846848554
        \\5283751526
    );
    try expect(result == 195);
}
