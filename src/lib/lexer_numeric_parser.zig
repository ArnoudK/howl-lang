const std = @import("std");
const LexerFile = @import("./lexer_enhanced.zig").LexerFile;
const Token = @import("./token.zig").Token;

pub const NumericParseError = error{
    InvalidDigitForBase,
    IntegerLiteralTooLarge,
    InvalidFloatFormat,
    UnexpectedEndOfNumber,
    InvalidBinaryLiteral,
    InvalidHexLiteral,
    InvalidDecimalLiteral,
    MissingDigitsAfterPrefix,
    InvalidExponentFormat,
    UnderscoreAtInvalidPosition,
};

pub fn parseDecimalNumber(self: *LexerFile, start_pos: usize) !void {
    var value: i256 = 0;
    var is_float = false;
    var has_digits = false;
    var last_was_underscore = false;

    // Parse integer part
    while (isDigitOrUnderscore(self, self.tokenize_state.char)) {
        if (self.tokenize_state.char == '_') {
            if (last_was_underscore or !has_digits) {
                return NumericParseError.UnderscoreAtInvalidPosition;
            }
            last_was_underscore = true;
        } else {
            const digit = self.tokenize_state.char - '0';
            if (value > std.math.maxInt(i256) / 10) {
                return NumericParseError.IntegerLiteralTooLarge;
            }
            value = value * 10 + digit;
            has_digits = true;
            last_was_underscore = false;
        }
        self.advance();
    }

    if (last_was_underscore) {
        return NumericParseError.UnderscoreAtInvalidPosition;
    }

    // Check for decimal point
    if (self.tokenize_state.char == '.') {
        is_float = true;
        self.advance();

        // Parse fractional part
        var fractional_value: f64 = 0;
        var fractional_divisor: f64 = 1;
        var has_fractional_digits = false;
        last_was_underscore = false;

        while (isDigitOrUnderscore(self, self.tokenize_state.char)) {
            if (self.tokenize_state.char == '_') {
                if (last_was_underscore or !has_fractional_digits) {
                    return NumericParseError.UnderscoreAtInvalidPosition;
                }
                last_was_underscore = true;
            } else {
                const digit = self.tokenize_state.char - '0';
                fractional_divisor *= 10;
                fractional_value = fractional_value * 10 + @as(f64, @floatFromInt(digit));
                has_fractional_digits = true;
                last_was_underscore = false;
            }
            self.advance();
        }

        if (last_was_underscore) {
            return NumericParseError.UnderscoreAtInvalidPosition;
        }

        // Parse scientific notation if present
        if (self.tokenize_state.char == 'e' or self.tokenize_state.char == 'E') {
            self.advance();

            var exponent_sign: i64 = 1;
            if (self.tokenize_state.char == '+' or self.tokenize_state.char == '-') {
                if (self.tokenize_state.char == '-') exponent_sign = -1;
                self.advance();
            }

            var exponent: i64 = 0;
            var has_exponent_digits = false;
            last_was_underscore = false;

            while (isDigitOrUnderscore(self, self.tokenize_state.char)) {
                if (self.tokenize_state.char == '_') {
                    if (last_was_underscore or !has_exponent_digits) {
                        return NumericParseError.UnderscoreAtInvalidPosition;
                    }
                    last_was_underscore = true;
                } else {
                    const digit = self.tokenize_state.char - '0';
                    exponent = exponent * 10 + @as(i64, @intCast(digit));
                    has_exponent_digits = true;
                    last_was_underscore = false;
                }
                self.advance();
            }

            if (!has_exponent_digits or last_was_underscore) {
                return NumericParseError.InvalidExponentFormat;
            }

            const pow = std.math.pow(f64, 10, @floatFromInt(exponent * exponent_sign));
            const final_value: f128 = (@as(f128, @floatFromInt(value)) + fractional_value / fractional_divisor) * pow;
            try self.tokens.append(Token{ .FloatLiteral = .{ .pos = start_pos, .value = @as(f128, final_value) } });
        } else {
            const final_value = @as(f128, @floatFromInt(value)) + fractional_value / fractional_divisor;
            try self.tokens.append(Token{ .FloatLiteral = .{ .pos = start_pos, .value = @as(f128, final_value) } });
        }
    } else {
        // Check for scientific notation on integers
        if (self.tokenize_state.char == 'e' or self.tokenize_state.char == 'E') {
            is_float = true;
            self.advance();

            var exponent_sign: i64 = 1;
            if (self.tokenize_state.char == '+' or self.tokenize_state.char == '-') {
                if (self.tokenize_state.char == '-') exponent_sign = -1;
                self.advance();
            }

            var exponent: i64 = 0;
            var has_exponent_digits = false;
            last_was_underscore = false;

            while (isDigitOrUnderscore(self, self.tokenize_state.char)) {
                if (self.tokenize_state.char == '_') {
                    if (last_was_underscore or !has_exponent_digits) {
                        return NumericParseError.UnderscoreAtInvalidPosition;
                    }
                    last_was_underscore = true;
                } else {
                    const digit = self.tokenize_state.char - '0';
                    exponent = exponent * 10 + @as(i64, @intCast(digit));
                    has_exponent_digits = true;
                    last_was_underscore = false;
                }
                self.advance();
            }

            if (!has_exponent_digits or last_was_underscore) {
                return NumericParseError.InvalidExponentFormat;
            }

            const final_value = @as(f128, @floatFromInt(value)) * std.math.pow(f64, 10, @as(f64, @floatFromInt(exponent * exponent_sign)));
            try self.tokens.append(Token{ .FloatLiteral = .{ .pos = start_pos, .value = final_value } });
        } else {
            try self.tokens.append(Token{ .IntegerLiteral = .{ .pos = start_pos, .value = value } });
        }
    }
}

pub fn parseIntegerWithBase(self: *LexerFile, base: u8, start_pos: usize) !void {
    // Skip the '0' and base character ('b' or 'x')
    self.advance(); // skip '0'
    self.advance(); // skip 'b' or 'x'

    var value: i256 = 0;
    var has_digits = false;
    var last_was_underscore = false;

    while (isValidDigitForBase(self, self.tokenize_state.char, base) or self.tokenize_state.char == '_') {
        if (self.tokenize_state.char == '_') {
            if (last_was_underscore or !has_digits) {
                return NumericParseError.UnderscoreAtInvalidPosition;
            }
            last_was_underscore = true;
        } else {
            const digit_value = try getDigitValue(self, self.tokenize_state.char, base);
            const base_i256 = @as(i256, @intCast(base));

            // Check for overflow before multiplication
            if (value > @divFloor((std.math.maxInt(i256) - digit_value), base_i256)) {
                return NumericParseError.IntegerLiteralTooLarge;
            }

            value = value * base_i256 + digit_value;
            has_digits = true;
            last_was_underscore = false;
        }
        self.advance();
    }

    if (!has_digits) {
        return switch (base) {
            2 => NumericParseError.MissingDigitsAfterPrefix,
            16 => NumericParseError.MissingDigitsAfterPrefix,
            else => NumericParseError.InvalidDecimalLiteral,
        };
    }

    if (last_was_underscore) {
        return NumericParseError.UnderscoreAtInvalidPosition;
    }

    try self.tokens.append(Token{ .IntegerLiteral = .{ .pos = start_pos, .value = value } });
}

fn isDigitOrUnderscore(self: *LexerFile, ch: u8) bool {
    _ = self;
    return (ch >= '0' and ch <= '9') or ch == '_';
}

fn isValidDigitForBase(self: *LexerFile, ch: u8, base: u8) bool {
    _ = self;
    return switch (base) {
        2 => ch == '0' or ch == '1',
        16 => (ch >= '0' and ch <= '9') or (ch >= 'a' and ch <= 'f') or (ch >= 'A' and ch <= 'F'),
        10 => ch >= '0' and ch <= '9',
        else => false,
    };
}

fn getDigitValue(self: *LexerFile, ch: u8, base: u8) NumericParseError!i256 {
    _ = self;
    return switch (ch) {
        '0'...'9' => ch - '0',
        'a'...'f' => if (base == 16) ch - 'a' + 10 else NumericParseError.InvalidDigitForBase,
        'A'...'F' => if (base == 16) ch - 'A' + 10 else NumericParseError.InvalidDigitForBase,
        else => NumericParseError.InvalidDigitForBase,
    };
}

fn powF128(base: f128, exponent: f128) f128 {
    if (exponent == 0) return 1;
    if (exponent == 1) return base;

    // Handle negative exponents
    if (exponent < 0) {
        return 1.0 / powF128(base, -exponent);
    }

    // For simplicity, convert to f64 and use std.math.pow, then convert back
    // This should be sufficient for most practical use cases

    const result_f128 = std.math.pow(f128, base, exponent);
    return result_f128;
}
