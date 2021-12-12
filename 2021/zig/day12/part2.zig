const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

const Cave = struct {
    identifier: []const u8,
    connections: std.ArrayList([]const u8),
    visits: isize = 0,

    const Self = @This();

    fn init(allocator: *std.mem.Allocator, map: *Map, identifier: []const u8) !void {
        var entry = try map.getOrPut(identifier);
        if (!entry.found_existing) {
            entry.value_ptr.* = Self{
                .identifier = try std.mem.dupe(allocator, u8, identifier),
                .connections = std.ArrayList([]const u8).init(allocator),
            };
        }
    }

    fn connect(cave1: *Cave, cave2: *Cave) !void {
        try cave1.connections.append(cave2.identifier);
        try cave2.connections.append(cave1.identifier);
    }

    fn deinit(self: *Self, allocator: *std.mem.Allocator) void {
        allocator.free(self.identifier);
        self.connections.deinit();
    }

    fn is_big(self: *const Self) bool {
        return self.identifier[0] >= 'A' and self.identifier[0] <= 'Z';
    }

    fn is_small(self: *const Self) bool {
        return !self.is_big();
    }

    fn is_start_or_end(self: *const Self) bool {
        return std.mem.eql(u8, self.identifier, "start") or std.mem.eql(u8, self.identifier, "end");
    }

    fn can_visit(self: *const Self, map: *Map) bool {
        return self.is_big() or self.visits == 0 or (!self.is_start_or_end() and !has_visited_small_cave_twice(map));
    }
};

const Map = std.StringHashMap(Cave);

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 16813);
    const result = try run(allocator, input);
    try stdout.print("{}\n", .{result});
}

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !isize {
    var map = Map.init(allocator);
    defer {
        var caveIter = map.iterator();
        while (caveIter.next()) |entry| {
            //allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit(allocator);
        }
        map.deinit();
    }

    var lines = std.mem.tokenize(input, "\n");
    while (lines.next()) |line| {
        var parts = std.mem.tokenize(line, "-");
        const id1 = parts.next().?;
        const id2 = parts.next().?;
        try Cave.init(allocator, &map, id1);
        try Cave.init(allocator, &map, id2);
        try Cave.connect(map.getPtr(id1).?, map.getPtr(id2).?);
    }

    return navigate(&map, "start", "end");
}

fn has_visited_small_cave_twice(map: *Map) bool {
    var cave_iter = map.valueIterator();
    return while (cave_iter.next()) |cave| {
        if (cave.is_small() and cave.visits > 1) {
            break true;
        }
    } else false;
}

fn navigate(map: *Map, here_id: []const u8, dest_id: []const u8) isize {
    if (std.mem.eql(u8, here_id, dest_id)) {
        return 1;
    }
    var n: isize = 0;
    var here = map.getPtr(here_id).?;
    here.visits += 1;
    for (here.connections.items) |connection_id| {
        var connection = map.getPtr(connection_id).?;
        if (connection.can_visit(map)) {
            n += navigate(map, connection_id, dest_id);
        }
    }
    here.visits -= 1;
    return n;
}

test "example scenario 1" {
    const result = try run(std.testing.allocator,
        \\start-A
        \\start-b
        \\A-c
        \\A-b
        \\b-d
        \\A-end
        \\b-end
    );
    try expect(result == 36);
}

test "example scenario 2" {
    const result = try run(std.testing.allocator,
        \\dc-end
        \\HN-start
        \\start-kj
        \\dc-start
        \\dc-HN
        \\LN-dc
        \\HN-end
        \\kj-sa
        \\kj-HN
        \\kj-dc
    );
    try expect(result == 103);
}
