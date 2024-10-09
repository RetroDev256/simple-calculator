const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const alloc = std.heap.page_allocator;

    while (true) {
        repl(stdin, stdout, alloc) catch |err| {
            const message = switch (err) {
                error.Unexpected => "unexpected character in floating point number",
                error.UnexpectedByte => "malformed input, not a valid math expression",
                error.UnexpectedToken => "malformed mathematical expression",
                else => return err,
            };
            try stdout.print("Error: {s}\n", .{message});
        } orelse break;
    }
}

fn repl(stdin: anytype, stdout: anytype, alloc: Allocator) !?void {
    try stdout.writeAll("Enter expression: ");
    const source = try input(alloc, stdin) orelse return null;

    // Read
    var toker: Tokenizer = .init(source);
    var token_list: std.ArrayListUnmanaged(Token) = .empty;
    defer token_list.deinit(alloc);
    read: while (true) {
        const tok = try toker.next();
        try token_list.append(alloc, tok);
        if (tok == .eof) break :read;
    }

    // Eval
    const tokens = try token_list.toOwnedSlice(alloc);
    defer alloc.free(tokens);
    var parser: Parser = .init(tokens);
    const result = try parser.parse();

    // Print
    try stdout.print("> Result: {d}\n", .{result});
}

// helper function to get user input
fn input(alloc: Allocator, reader: anytype) !?[:0]const u8 {
    var in: std.ArrayListUnmanaged(u8) = .empty;
    defer in.deinit(alloc);
    const in_writer = in.writer(alloc);
    // read user result, break on "enter"
    if (reader.streamUntilDelimiter(in_writer, '\n', null)) |_| {
        return try in.toOwnedSliceSentinel(alloc, 0);
    } else |err| switch (err) {
        error.EndOfStream => return null,
        else => return err,
    }
}

const Tag = enum {
    eof,
    l_paren,
    r_paren,
    add,
    sub,
    mul,
    div,
    exp,
    number,
};

const Token = union(Tag) {
    eof,
    l_paren,
    r_paren,
    add,
    sub,
    mul,
    div,
    exp,
    number: []const u8,
};

const Tokenizer = struct {
    source: [:0]const u8,
    index: usize,

    pub fn init(source: [:0]const u8) @This() {
        return .{ .source = source, .index = 0 };
    }

    pub fn next(self: *@This()) !Token {
        scan: switch (self.source[self.index]) {
            0 => return .eof,
            ' ', '\t', '\r', '\n' => {
                self.index += 1;
                continue :scan self.source[self.index];
            },
            '(', ')', '+', '-', '*', '/', '^' => |byte| {
                self.index += 1;
                return switch (byte) {
                    '(' => .l_paren,
                    ')' => .r_paren,
                    '+' => .add,
                    '-' => .sub,
                    '*' => .mul,
                    '/' => .div,
                    '^' => .exp,
                    else => unreachable,
                };
            },
            '0'...'9', '.' => {
                const start = self.index;
                while (true) {
                    self.index += 1;
                    switch (self.source[self.index]) {
                        '0'...'9', '.' => continue,
                        else => {
                            const number = self.source[start..self.index];
                            return .{ .number = number };
                        },
                    }
                }
            },
            else => return error.UnexpectedByte,
        }
    }
};

const Parser = struct {
    // delimited by .eof
    source: []const Token,
    index: usize = 0,

    pub fn init(source: []const Token) @This() {
        return .{ .source = source, .index = 0 };
    }

    const ParseError = std.fmt.ParseFloatError || error{UnexpectedToken};

    // evaluates an expression and returns the result
    pub fn parse(self: *@This()) ParseError!f64 {
        const result = try self.expression();
        try self.consume(.eof);
        return result;
    }

    // <expression> ::= <term> (("+" | "-") <term>)*
    fn expression(self: *@This()) ParseError!f64 {
        var result: f64 = try self.term();
        scan: switch (self.source[self.index]) {
            .add => {
                try self.consume(.add);
                result += try self.term();
                continue :scan self.source[self.index];
            },
            .sub => {
                try self.consume(.sub);
                result -= try self.term();
                continue :scan self.source[self.index];
            },
            else => return result,
        }
    }

    // <term> ::= <factor> (("*" | "/") <factor>)*
    fn term(self: *@This()) ParseError!f64 {
        var result: f64 = try self.factor();
        scan: switch (self.source[self.index]) {
            .mul => {
                try self.consume(.mul);
                result *= try self.factor();
                continue :scan self.source[self.index];
            },
            .div => {
                try self.consume(.div);
                result /= try self.factor();
                continue :scan self.source[self.index];
            },
            else => return result,
        }
    }

    // factor ::= negation ('^' factor)*
    fn factor(self: *@This()) ParseError!f64 {
        var result: f64 = try self.negation();
        if (self.source[self.index] == .exp) {
            try self.consume(.exp);
            const power = try self.factor();
            result = std.math.pow(f64, result, power);
        }
        return result;
    }

    // <negation> ::= "-" <base> | <base>
    fn negation(self: *@This()) ParseError!f64 {
        if (self.source[self.index] == .sub) {
            try self.consume(.sub);
            return -try self.number();
        }
        return try self.number();
    }

    // number ::= '(' expression ')' | <floating point number>
    fn number(self: *@This()) ParseError!f64 {
        switch (self.source[self.index]) {
            .l_paren => {
                try self.consume(.l_paren);
                const result = try self.expression();
                try self.consume(.r_paren);
                return result;
            },
            .number => |num_str| {
                try self.consume(.number);
                return try std.fmt.parseFloat(f64, num_str);
            },
            else => return error.UnexpectedToken,
        }
    }

    // asserts the matching token tag and advances to the next
    fn consume(self: *@This(), tag: Tag) ParseError!void {
        if (self.source[self.index] != tag) {
            return error.UnexpectedToken;
        }
        self.index += 1;
    }
};
