const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const alloc = std.heap.page_allocator;

    var state: State = .init;

    while (true) {
        replOnce(stdin, stdout, alloc, &state) catch |err| {
            const message = switch (err) {
                error.Unexpected => "unexpected character in floating point number",
                error.UnexpectedByte => "malformed input, not a valid math expression",
                error.UnexpectedToken => "malformed mathematical expression",
                error.VariableNotDefined => "invalid variable, constant, or function name",
                error.VariableShadowsConstant => "variable name shadows mathematical constant",
                error.VariableShadowsFunction => "variable name shadows mathematical function",
                else => return err,
            };
            try stdout.print("Error: {s}\n", .{message});
        } orelse break;
    }
}

const Complex = std.math.complex.Complex(f64);

const State = struct {
    previous: Complex,
    variables: std.StringHashMapUnmanaged(Complex),

    pub const init: @This() = .{
        .previous = .init(0, 0),
        .variables = .empty,
    };
};

fn replOnce(stdin: anytype, stdout: anytype, alloc: Allocator, state: *State) !?void {
    try stdout.writeAll("Expr: ");
    const source = try input(alloc, stdin) orelse return null;

    // Program termination
    if (std.StaticStringMap(void).initComptime(
        .{ .{"quit"}, .{"exit"}, .{"q"}, .{"bye"} },
    ).has(source)) {
        return null;
    }

    // Read & Tokenize
    var toker: Tokenizer = .init(source);
    var token_list: std.ArrayListUnmanaged(Token) = .empty;
    defer token_list.deinit(alloc);
    read: while (true) {
        const tok = try toker.next();
        try token_list.append(alloc, tok);
        if (tok == .eof) break :read;
    }

    // Parse & Evaluate
    const tokens = try token_list.toOwnedSlice(alloc);
    defer alloc.free(tokens);
    var parser: Parser = .init(tokens, state, alloc);
    if (try parser.parse()) |result| {
        // Update the state
        state.previous = result;
        // Print the evaluation
        try stdout.writeAll("=> ");
        if (result.im == 0) {
            try stdout.print("{d}\n", .{result.re});
        } else if (result.im < 0) {
            try stdout.print("{d} - {d}i\n", .{ result.re, -result.im });
        } else {
            try stdout.print("{d} + {d}i\n", .{ result.re, result.im });
        }
    }
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

const Token = union(enum) {
    // Special
    eof, // \x00
    equal, // =

    // Level 0
    add, // +
    sub, // -

    // Level 1
    mul, // *
    div, // /

    // Level 2
    pow, // ^

    // Level 3
    l_paren, // (
    r_paren, // )
    conjugate, // conj / conjugate
    real, // re / real
    imag, // im / imag
    sqrt, // sqrt
    log10, // log
    loge, // ln
    log2, // lb
    exp, // exp
    sin, // sin
    cos, // cos
    tan, // tan
    abs, // abs / mag
    sinh, // sinh
    cosh, // cosh
    tanh, // tanh
    asin, // asin
    acos, // acos
    atan, // atan
    asinh, // asinh
    acosh, // acosh
    atanh, // atanh
    ceil, // ceil
    floor, // floor
    gamma, // gamma

    // Level 4
    previous, // .
    number: Complex, // 12345
    variable: []const u8, // dead_beef

    const constant_map = std.StaticStringMap(Complex).initComptime(.{
        .{ "i", Complex.init(0, 1) },
        .{ "e", Complex.init(std.math.e, 0) },
        .{ "pi", Complex.init(std.math.pi, 0) },
        .{ "phi", Complex.init(std.math.phi, 0) },
        .{ "tau", Complex.init(std.math.tau, 0) },
    });

    const function_map = std.StaticStringMap(Token).initComptime(.{
        .{ "conj", .conjugate },
        .{ "conjugate", .conjugate },
        .{ "re", .real },
        .{ "im", .imag },
        .{ "real", .real },
        .{ "imag", .imag },
        .{ "sqrt", .sqrt },
        .{ "log", .log10 },
        .{ "ln", .loge },
        .{ "lb", .log2 },
        .{ "exp", .exp },
        .{ "mag", .abs },
        .{ "abs", .abs },
        .{ "sin", .sin },
        .{ "cos", .cos },
        .{ "tan", .tan },
        .{ "sinh", .sinh },
        .{ "cosh", .cosh },
        .{ "tanh", .tanh },
        .{ "asin", .asin },
        .{ "acos", .acos },
        .{ "atan", .atan },
        .{ "asinh", .asinh },
        .{ "acosh", .acosh },
        .{ "atanh", .atanh },
        .{ "ceil", .ceil },
        .{ "floor", .floor },
        .{ "gamma", .gamma },
    });
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
                    '^' => .pow,
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
                            if (self.source[start] == '.' and self.index == start + 1) {
                                return .previous;
                            } else {
                                const real_str = self.source[start..self.index];
                                const real = try std.fmt.parseFloat(f64, real_str);
                                return .{ .number = .init(real, 0) };
                            }
                        },
                    }
                }
            },
            'A'...'Z', 'a'...'z', '_' => {
                const start = self.index;
                while (true) {
                    self.index += 1;
                    switch (self.source[self.index]) {
                        'A'...'Z', 'a'...'z', '_' => continue,
                        else => {
                            const identifier = self.source[start..self.index];
                            if (Token.constant_map.get(identifier)) |constant| {
                                return .{ .number = constant };
                            } else if (Token.function_map.get(identifier)) |function| {
                                return function;
                            } else {
                                return .{ .variable = identifier };
                            }
                        },
                    }
                }
            },
            '=' => {
                self.index += 1;
                return .equal;
            },
            else => return error.UnexpectedByte,
        }
    }
};

const Parser = struct {
    // delimited by .eof
    source: []const Token,
    index: usize,
    state: *State,
    allocator: Allocator,

    pub fn init(source: []const Token, state: *State, allocator: Allocator) @This() {
        return .{
            .source = source,
            .index = 0,
            .state = state,
            .allocator = allocator,
        };
    }

    const ParseError = error{
        UnexpectedToken,
        VariableNotDefined,
        VariableShadowsConstant,
        VariableShadowsFunction,
    } || Allocator.Error;

    // evaluates an expression and returns the result
    pub fn parse(self: *@This()) ParseError!?Complex {
        const result = try self.root();
        try self.consume(.eof);
        return result;
    }

    // <root> ::= <variable> <equal> <expression> | <expression>
    fn root(self: *@This()) ParseError!?Complex {
        if (self.source[self.index] == .variable) {
            if (self.source[self.index + 1] == .equal) {
                const name = self.source[self.index].variable;
                if (Token.constant_map.has(name)) {
                    return error.VariableShadowsConstant;
                } else if (Token.function_map.has(name)) {
                    return error.VariableShadowsFunction;
                } else {
                    self.index += 2;
                    const value = try self.expression();
                    try self.state.variables.put(self.allocator, name, value);
                    return null;
                }
            }
        }
        return try self.expression();
    }

    // <expression> ::= <term> (("+" | "-") <term>)*
    fn expression(self: *@This()) ParseError!Complex {
        var result: Complex = try self.term();
        scan: switch (self.source[self.index]) {
            .add => {
                self.index += 1;
                result = result.add(try self.term());
                continue :scan self.source[self.index];
            },
            .sub => {
                self.index += 1;
                result = result.sub(try self.term());
                continue :scan self.source[self.index];
            },
            else => return result,
        }
    }

    // <term> ::= <factor> (("*" | "/") <factor>)* | <factor> <factor>
    fn term(self: *@This()) ParseError!Complex {
        var result: Complex = try self.factor();
        scan: switch (self.source[self.index]) {
            .mul => {
                self.index += 1;
                result = result.mul(try self.factor());
                continue :scan self.source[self.index];
            },
            .div => {
                self.index += 1;
                result = result.div(try self.factor());
                continue :scan self.source[self.index];
            },
            else => {
                // implied multiplication
                if (self.factor()) |other| {
                    return result.mul(other);
                } else |_| {
                    return result;
                }
            },
        }
    }

    // <factor> ::= <negation> ('^' <factor>)*
    fn factor(self: *@This()) ParseError!Complex {
        var result: Complex = try self.negation();
        if (self.source[self.index] == .pow) {
            self.index += 1;
            const power = try self.factor();
            result = std.math.complex.pow(result, power);
        }
        return result;
    }

    // <negation> ::= "-" <base> | <base>
    fn negation(self: *@This()) ParseError!Complex {
        if (self.source[self.index] == .sub) {
            self.index += 1;
            return (try self.number()).neg();
        }
        return try self.number();
    }

    // <number> ::= '(' <expression> ')' | <floating point number> | <function>
    fn number(self: *@This()) ParseError!Complex {
        switch (self.source[self.index]) {
            .l_paren => {
                self.index += 1;
                const result = try self.expression();
                try self.consume(.r_paren);
                return result;
            },
            .number => |num| {
                self.index += 1;
                return num;
            },
            .previous => {
                self.index += 1;
                return self.state.previous;
            },
            .variable => |name| {
                self.index += 1;
                if (self.state.variables.get(name)) |value| {
                    return value;
                } else {
                    return error.VariableNotDefined;
                }
            },
            else => return try self.function(),
        }
    }

    // <function> ::= <function type> <number>
    fn function(self: *@This()) ParseError!Complex {
        const complex = std.math.complex;
        const log10 = complex.log(Complex.init(10, 0));
        const log2 = complex.log(Complex.init(2, 0));
        const token = self.source[self.index];
        switch (token) {
            .conjugate,
            .real,
            .imag,
            .sqrt,
            .log10,
            .loge,
            .log2,
            .exp,
            .abs,
            .sin,
            .cos,
            .tan,
            .sinh,
            .cosh,
            .tanh,
            .asin,
            .acos,
            .atan,
            .asinh,
            .acosh,
            .atanh,
            .ceil,
            .floor,
            .gamma,
            => {
                self.index += 1;
                const num = try self.number();
                return switch (token) {
                    .conjugate => num.conjugate(),
                    .real => .init(num.re, 0),
                    .imag => .init(num.im, 0),
                    .sqrt => complex.sqrt(num),
                    .log10 => complex.log(num).div(log10),
                    .loge => complex.log(num),
                    .log2 => complex.log(num).div(log2),
                    .exp => complex.exp(num),
                    .abs => .init(complex.abs(num), 0),
                    .sin => complex.sin(num),
                    .cos => complex.cos(num),
                    .tan => complex.tan(num),
                    .sinh => complex.sinh(num),
                    .cosh => complex.cosh(num),
                    .tanh => complex.tanh(num),
                    .asin => complex.asin(num),
                    .acos => complex.acos(num),
                    .atan => complex.atan(num),
                    .asinh => complex.asinh(num),
                    .acosh => complex.acosh(num),
                    .atanh => complex.atanh(num),
                    .ceil => .init(@ceil(num.re), @ceil(num.im)),
                    .floor => .init(@floor(num.re), @floor(num.im)),
                    .gamma => blk: {
                        const gamma_re = std.math.gamma(f64, num.re);
                        const gamma_im = std.math.gamma(f64, num.im);
                        const actual_im = if (num.im == 0) 0 else gamma_im;
                        break :blk .init(gamma_re, actual_im);
                    },
                    else => unreachable,
                };
            },
            else => return error.UnexpectedToken,
        }
    }

    // checks that the matching token tag and advances to the next
    fn consume(self: *@This(), tag: std.meta.Tag(Token)) ParseError!void {
        if (self.source[self.index] != tag) {
            return error.UnexpectedToken;
        }
        self.index += 1;
    }
};
