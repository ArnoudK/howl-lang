const std = @import("std");
/// The Zig errors that the CodeGen can cause
/// DO NOT EDIT
pub const CompileError = error{
    CCompilationFailed,
    OutOfMemory,
    InvalidNumberFormat,
    DuplicateSymbol,
    UnsupportedAstNode,
    InvalidNodeReference,
    OptimizationFailed,
    NodeHasUsers,
    CyclicGraph,
    InvalidTransformation,
    InvalidNode,
    UnsupportedType,
    NodeValueNotFound,
} || std.fs.Dir.MakeError || std.fs.File.ReadError || std.fs.File.WriteError || std.fs.File.WriteFileError || std.fs.File.OpenError ||
    std.process.Child.RunError;
