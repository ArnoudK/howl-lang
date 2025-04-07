const TokenKinds = @import("tokenkinds.zig").TokenKinds;
const std = @import("std");

pub const TokenError = error{
    InvalidIntegerSuffix,
    InvalidIntOctalDigit,
    InvalidIntBinaryDigit,
    InvalidIntHexDigit,
    InvalidIntBaseTenDigit,

    InvalidFloatSuffix,
};

pub const LexerToken = struct {
    const Self = @This();
    kind: TokenKinds,
    filePos: usize,
    value: ?[]const u8,

    pub fn init(kind: TokenKinds, filePos: usize, value: ?[]const u8) LexerToken {
        return LexerToken{ .kind = kind, .filePos = filePos, .value = value };
    }

    // convert the value to an integer
    // handles 0b 0o 0x prefixes as well as suffixes
    // ignores _ separators
    pub fn getAsIntValue(self: Self) TokenError!i128 {
        std.debug.assert(self.kind == TokenKinds.NumberLiteral);
        std.debug.assert(self.value != null);

        var intVal: i128 = 0;
        var base: usize = 10;
        const valLen = self.value.?.len;
        var i: usize = 0;
        if (valLen >= 2) {
            if (self.value.?[0] == '0') {
                if (self.value.?[1] == 'b') {
                    base = 2;
                    i = 2;
                } else if (self.value.?[1] == 'o') {
                    base = 8;
                    i = 2;
                } else if (self.value.?[1] == 'x') {
                    base = 16;
                    i = 2;
                }
            }
        }
        switch (base) {
            2 => {
                while (i < valLen) {
                    const c = self.value.?[i];
                    if (c == '_') {
                        i += 1;
                        continue;
                    }

                    // Check for suffix
                    if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z')) {
                        break;
                    }

                    if (c == '0') {
                        intVal = intVal * 2;
                    } else if (c == '1') {
                        intVal = intVal * 2 + 1;
                    } else {
                        return TokenError.InvalidIntBinaryDigit;
                    }
                    i += 1;
                }
            },
            8 => {
                while (i < valLen) {
                    const c = self.value.?[i];
                    if (c == '_') {
                        i += 1;
                        continue;
                    }

                    // Check for suffix
                    if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z')) {
                        break;
                    }

                    if (c >= '0' and c <= '7') {
                        intVal = intVal * 8 + (c - '0');
                    } else {
                        return TokenError.InvalidIntOctalDigit;
                    }
                    i += 1;
                }
            },
            10 => {
                while (i < valLen) {
                    const c = self.value.?[i];
                    if (c == '_') {
                        i += 1;
                        continue;
                    }

                    // Check for suffix
                    if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z')) {
                        break;
                    }

                    if (c >= '0' and c <= '9') {
                        intVal = intVal * 10 + (c - '0');
                    } else {
                        return TokenError.InvalidIntBaseTenDigit;
                    }
                    i += 1;
                }
            },
            16 => {
                while (i < valLen) {
                    const c = self.value.?[i];
                    if (c == '_') {
                        i += 1;
                        continue;
                    }

                    // For hex, check if it's a suffix (letter after f/F)
                    if ((c >= 'a' and c <= 'z' and c > 'f') or
                        (c >= 'A' and c <= 'Z' and c > 'F'))
                    {
                        break;
                    }

                    var digit: u8 = 0;
                    if (c >= '0' and c <= '9') {
                        digit = c - '0';
                    } else if (c >= 'a' and c <= 'f') {
                        digit = c - 'a' + 10;
                    } else if (c >= 'A' and c <= 'F') {
                        digit = c - 'A' + 10;
                    } else {
                        return TokenError.InvalidIntHexDigit;
                    }

                    intVal = intVal * 16 + digit;
                    i += 1;
                }
            },
            else => {
                unreachable;
            },
        }

        return intVal;
    }

    // convert the value to a float
    // handles 3e10 => 3 * 10^10
    // ignores _ separators
    pub fn getAsFloatValue(self: Self) TokenError!f128 {
        std.debug.assert(self.kind == TokenKinds.FloatLiteral);
        std.debug.assert(self.value != null);

        const value = self.value.?;
        const len = value.len;
        var i: usize = 0;

        // Parse mantissa (before 'e' or 'E')
        var result: f128 = 0.0;
        var decimal_seen = false;
        var decimal_places: f128 = 1.0;

        // Skip any prefix like 0x, 0o, 0b - not typically used for floats
        // but we'll handle it for consistency
        if (len >= 2 and value[0] == '0') {
            if (value[1] == 'x' or value[1] == 'b' or value[1] == 'o') {
                i = 2;
            }
        }

        // Parse digits before and after decimal point
        while (i < len) {
            const c = value[i];

            // Skip underscores
            if (c == '_') {
                i += 1;
                continue;
            }

            // Parse decimal point
            if (c == '.') {
                if (decimal_seen) {
                    return TokenError.InvalidFloatSuffix; // Multiple decimal points
                }
                decimal_seen = true;
                i += 1;
                continue;
            }

            // Check for exponent
            if (c == 'e' or c == 'E') {
                break;
            }

            // Check for suffix
            if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z')) {
                break;
            }

            // Parse digit
            if (c >= '0' and c <= '9') {
                if (decimal_seen) {
                    decimal_places *= 10.0;
                    result += @as(f128, @floatFromInt(c - '0')) / decimal_places;
                } else {
                    result = result * 10.0 + @as(f128, @floatFromInt(c - '0'));
                }
            } else {
                return TokenError.InvalidFloatSuffix;
            }

            i += 1;
        }

        // Parse exponent if present
        if (i < len and (value[i] == 'e' or value[i] == 'E')) {
            i += 1; // Skip 'e' or 'E'

            // Handle optional sign
            var exp_sign: f128 = 1.0;
            if (i < len and (value[i] == '+' or value[i] == '-')) {
                if (value[i] == '-') {
                    exp_sign = -1.0;
                }
                i += 1;
            }

            // Parse exponent digits
            var exponent: i32 = 0;
            var exp_digit_seen = false;

            while (i < len) {
                const c = value[i];

                // Skip underscores
                if (c == '_') {
                    i += 1;
                    continue;
                }

                // Check for suffix
                if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z')) {
                    break;
                }

                if (c >= '0' and c <= '9') {
                    exponent = exponent * 10 + @as(i32, @intCast(c - '0'));
                    exp_digit_seen = true;
                } else {
                    return TokenError.InvalidFloatSuffix;
                }

                i += 1;
            }

            if (!exp_digit_seen) {
                return TokenError.InvalidFloatSuffix; // No digits in exponent
            }

            // Apply exponent
            const pow: i128 = std.math.pow(i128, 10, exponent);
            result *= @floatFromInt(pow);
        }

        // Check if we've consumed the entire token (ignoring any valid suffix)
        // For now, we're assuming we don't need to validate any suffix here

        return result;
    }
};
