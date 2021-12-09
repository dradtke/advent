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
    const segments = "abcdefg";

    var permutations = std.ArrayList([]const u8).init(allocator);
    defer {
        for (permutations.items) |item| {
            // TODO: this line breaks the real run, but is needed for running tests.
            // Not entirely sure why, honestly.
            // std.testing.allocator.free(item);
        }
        permutations.deinit();
    }
    try permute(allocator, &permutations, segments, 0, comptime segments.len - 1);

    var sum: isize = 0;

    var lines = std.mem.tokenize(input, "\n");
    while (lines.next()) |line| {
        var display = [_]u8{0} ** 4;

        var halves = std.mem.tokenize(line, "|");
        var unique_patterns = std.ArrayList([]const u8).init(allocator);
        defer unique_patterns.deinit();

        var unique_pattern_iter = std.mem.tokenize(halves.next().?, " ");
        while (unique_pattern_iter.next()) |pattern| {
            try unique_patterns.append(pattern);
        }

        var mapping = std.AutoHashMap(u8, u8).init(allocator);
        defer mapping.deinit();

        for (permutations.items) |permutation| {
            mapping.clearRetainingCapacity();
            comptime var i = 0;
            inline while (i < segments.len) : (i += 1) {
                try mapping.put(permutation[i], segments[i]);
            }
            if (try is_valid_mapping(allocator, unique_patterns.items, mapping)) {
                break;
            }
        } else unreachable;

        var output_values_iter = std.mem.tokenize(halves.next().?, " ");

        comptime var i = 0;
        inline while (i < 4) : (i += 1) {
            display[i] = (try get_mapped_digit(allocator, output_values_iter.next().?, mapping)).?;
        }

        sum += try std.fmt.parseInt(isize, display[0..display.len], 10);
    }

    return sum;
}

fn get_digit(allocator: *std.mem.Allocator, signals: []const u8) !?u8 {
    var sorted = try std.mem.dupe(allocator, u8, signals);
    defer allocator.free(sorted);

    std.sort.sort(u8, sorted, {}, comptime std.sort.asc(u8));
    if (std.mem.eql(u8, sorted, "abcefg")) {
        return '0';
    } else if (std.mem.eql(u8, sorted, "cf")) {
        return '1';
    } else if (std.mem.eql(u8, sorted, "acdeg")) {
        return '2';
    } else if (std.mem.eql(u8, sorted, "acdfg")) {
        return '3';
    } else if (std.mem.eql(u8, sorted, "bcdf")) {
        return '4';
    } else if (std.mem.eql(u8, sorted, "abdfg")) {
        return '5';
    } else if (std.mem.eql(u8, sorted, "abdefg")) {
        return '6';
    } else if (std.mem.eql(u8, sorted, "acf")) {
        return '7';
    } else if (std.mem.eql(u8, sorted, "abcdefg")) {
        return '8';
    } else if (std.mem.eql(u8, sorted, "abcdfg")) {
        return '9';
    } else {
        return null;
    }
}

fn get_mapped_digit(allocator: *std.mem.Allocator, signals: []const u8, mapping: std.AutoHashMap(u8, u8)) !?u8 {
    var mapped = try allocator.alloc(u8, signals.len);
    defer allocator.free(mapped);
    for (signals) |signal, i| {
        mapped[i] = mapping.get(signal).?;
    }
    return try get_digit(allocator, mapped);
}

fn is_valid_mapping(allocator: *std.mem.Allocator, signals_list: []const []const u8, mapping: std.AutoHashMap(u8, u8)) !bool {
    return for (signals_list) |signals| {
        const mapped = try get_mapped_digit(allocator, signals, mapping);
        if (mapped == null) {
            break false;
        }
    } else true;
}

fn permute(allocator: *std.mem.Allocator, results: *std.ArrayList([]const u8), string: []const u8, l: usize, r: usize) error{OutOfMemory}!void {
    var copy = try std.mem.dupe(allocator, u8, string);

    if (l == r) {
        try results.append(copy);
        return;
    }

    defer allocator.free(copy);

    var i: usize = l;
    while (i <= r) : (i += 1) {
        std.mem.swap(u8, &copy[l], &copy[i]);
        try permute(allocator, results, copy, l + 1, r);
        std.mem.swap(u8, &copy[l], &copy[i]);
    }
}

// the signal present in the 3-length one, but not 2-length, must be a
// the two values in the 4-length one, but not 2-length, are either b or d

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
        \\edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
        \\fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
        \\fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
        \\aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
        \\fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
        \\dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
        \\bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
        \\egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
        \\gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
    );
    std.debug.print("found result: {}\n", .{result});
    try expect(result == 61229);
}

//test "permute" {
//    var permutations = std.ArrayList([]const u8).init(std.testing.allocator);
//    defer {
//        for (permutations.items) |item| {
//            std.testing.allocator.free(item);
//        }
//        permutations.deinit();
//    }
//
//    try permute(std.testing.allocator, &permutations, "ABC", 0, 2);
//    try expect(permutations.items.len == 6);
//    try expect(std.mem.eql(u8, permutations.items[0], "ABC"));
//    try expect(std.mem.eql(u8, permutations.items[1], "ACB"));
//    try expect(std.mem.eql(u8, permutations.items[2], "BAC"));
//    try expect(std.mem.eql(u8, permutations.items[3], "BCA"));
//    try expect(std.mem.eql(u8, permutations.items[4], "CBA"));
//    try expect(std.mem.eql(u8, permutations.items[5], "CAB"));
//}
