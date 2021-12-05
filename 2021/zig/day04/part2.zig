const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const expect = std.testing.expect;

const Square = struct {
    number: isize,
    marked: bool,
};

const board_width = 5;
const board_height = 5;

const Row = [board_width]Square;
const Board = [board_height]Row;

const Result = struct {
    board: Board,
    last_drawn: isize,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const input = try stdin.readAllAlloc(allocator, 7890);
    const result = try run(allocator, input);
    try stdout.print("{}\n", .{result});
}

pub fn run(allocator: *std.mem.Allocator, input: []const u8) !isize {
    var lines = std.mem.tokenize(input, "\n");
    const drawn_numbers_line = std.mem.trim(u8, lines.next().?, " ");
    var drawn_numbers = std.mem.tokenize(drawn_numbers_line, ",");

    var boards = std.ArrayList(Board).init(allocator);
    defer boards.deinit();

    while (lines.next()) |first_line| {
        var board: Board = [_]Row{[_]Square{Square{
            .number = -1,
            .marked = false,
        }} ** board_width} ** board_height;
        try read_row(first_line, &board[0]);

        comptime var i = 1;
        inline while (i < board_height) : (i += 1) {
            const line = lines.next();
            try read_row(line.?, &board[i]);
        }

        try boards.append(board);
    }

    var results = std.ArrayList(Result).init(allocator);
    defer results.deinit();

    // play the game!
    while (drawn_numbers.next()) |raw_number| {
        const number = try std.fmt.parseInt(isize, raw_number, 10);

        for (boards.items) |*board| {
            mark_number(board, number);
        }

        removing_winners: while (true) {
            for (boards.items) |board, i| {
                if (has_won(board)) {
                    try results.append(Result{
                        .board = boards.swapRemove(i),
                        .last_drawn = number,
                    });
                    continue :removing_winners;
                }
            } else break;
        }

        if (boards.items.len == 0) {
            break;
        }
    }

    return calculate_score(results.items[results.items.len - 1]);
}

fn read_row(line: []const u8, row: *Row) !void {
    var numbers = std.mem.tokenize(line, " ");
    var i: usize = 0;
    while (numbers.next()) |number| {
        row.*[i].number = try std.fmt.parseInt(isize, number, 10);
        i += 1;
    }
}

fn has_won(board: Board) bool {
    {
        var row: usize = 0;
        while (row < board_height) : (row += 1) {
            var column: usize = 0;
            while (column < board_width) : (column += 1) {
                if (!board[row][column].marked) {
                    break;
                }
            } else return true;
        }
    }
    {
        var column: usize = 0;
        while (column < board_width) : (column += 1) {
            var row: usize = 0;
            while (row < board_height) : (row += 1) {
                if (!board[row][column].marked) {
                    break;
                }
            } else return true;
        }
    }
    return false;
}

fn mark_number(board: *Board, number: isize) void {
    var row: usize = 0;
    while (row < board_height) : (row += 1) {
        var column: usize = 0;
        while (column < board_width) : (column += 1) {
            var square = &(board[row][column]);
            if (square.*.number == number) {
                square.*.marked = true;
            }
        }
    }
}

fn calculate_score(result: Result) isize {
    var score: isize = 0;

    var row: usize = 0;
    while (row < board_height) : (row += 1) {
        var column: usize = 0;
        while (column < board_width) : (column += 1) {
            var square = result.board[row][column];
            if (!square.marked) {
                score += square.number;
            }
        }
    }

    return score * result.last_drawn;
}

fn print_board(board: Board) void {
    var row: usize = 0;
    while (row < board_height) : (row += 1) {
        var column: usize = 0;
        while (column < board_width) : (column += 1) {
            var square = board[row][column];
            if (square.marked) {
                std.debug.print("[{}] ", .{square.number});
            } else {
                std.debug.print(" {}  ", .{square.number});
            }
        }
        std.debug.print("\n", .{});
    }
    std.debug.print("\n", .{});
}

test "example scenario" {
    const result = try run(std.testing.allocator,
        \\7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
        \\
        \\22 13 17 11  0
        \\ 8  2 23  4 24
        \\21  9 14 16  7
        \\ 6 10  3 18  5
        \\ 1 12 20 15 19
        \\
        \\ 3 15  0  2 22
        \\ 9 18 13 17  5
        \\19  8  7 25 23
        \\20 11 10 24  4
        \\14 21 16 12  6
        \\
        \\14 21 17 24  4
        \\10 16 15  9 19
        \\18  8 23 26 20
        \\22 11 13  6  5
        \\ 2  0 12  3  7
    );
    try expect(result == 1924);
}
