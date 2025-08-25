# Sea-of-Nodes IR Implementation for Howl

## Overview

This implementation adds a sea-of-nodes intermediate representation (IR) to the Howl compiler pipeline, enabling advanced optimizations and cleaner code generation.

## New Compilation Pipeline

The compilation pipeline now follows this sequence:

```
Lexing → Parsing → Semantic Analysis → IR Construction → IR Optimization → CodeGen
```

### New Phases

1. **IR Construction** (`ir_construction`) - Transform AST to Sea-of-Nodes IR
2. **IR Optimization** (`ir_optimization`) - Run optimization passes on IR
3. **Modified CodeGen** (`codegen`) - Generate code from optimized IR

## Key Components

### Core IR System (`src/lib/sea_of_nodes_ir.zig`)

- **IrNode**: Individual nodes in the sea-of-nodes graph
- **SeaOfNodes**: Container for the IR graph with node management
- **IrOp**: Enumeration of all IR operations (control, data, memory)
- **IrConstant**: Compile-time constant values in IR

Key features:
- Memory-efficient node references using `IrNodeId`
- Explicit separation of control and data dependencies
- Built-in graph traversal and manipulation utilities
- Topological sorting for code generation

### AST-to-IR Transformation (`src/lib/ast_to_ir.zig`)

Transforms the semantically analyzed AST into sea-of-nodes IR:
- **Variable declarations** → `Alloc` + `Store` nodes
- **Variable references** → `Load` nodes  
- **Expressions** → Data flow graph with proper dependencies
- **Control flow** → Explicit `Region`/`If`/`Loop` control nodes

### Optimization Framework (`src/lib/ir_optimizer.zig`)

Implements a pass-based optimization system:
- **Constant Folding**: Evaluate compile-time constants
- **Dead Code Elimination**: Remove unreachable nodes
- **Common Subexpression Elimination**: Merge identical computations
- **Copy Propagation**: Replace loads of stored constants

### IR-Based Code Generators

#### JavaScript Generator (`src/lib/codegen_js_ir.zig`)
- Generates JavaScript directly from IR nodes
- Maintains variable mapping for IR nodes
- Produces clean, readable JavaScript output

#### C Generator (`src/lib/codegen_c_ir.zig`)  
- Generates C code directly from IR nodes
- Includes automatic compilation with filc
- Handles type mapping and variable declarations

## Benefits

### Optimization Advantages
- **Better optimization opportunities**: Explicit data/control flow representation
- **Target-agnostic optimizations**: All optimizations benefit both backends
- **Easier optimization implementation**: Uniform IR node representation
- **Composable passes**: Mix and match optimization strategies

### Code Generation Advantages
- **Cleaner code generation**: IR provides linearized, optimized representation
- **Better debugging info**: Source locations preserved through transformation
- **Simplified backends**: Code generators work with uniform IR structure

### Compiler Architecture Advantages
- **Clean separation of concerns**: Optimization separate from parsing/semantic analysis
- **Better error handling**: Clear failure modes with specific error messages
- **Extensible design**: Easy to add new optimizations and code generation targets

## Error Handling Philosophy

Following the "clear errors if anything goes wrong" approach:
- **No fallbacks**: Any IR construction/optimization failure immediately halts compilation
- **Clear error messages**: Specific error reporting with source locations
- **Deterministic compilation**: No partial states or unclear failure modes

## Integration with Existing System

The new IR system integrates cleanly with the existing compiler:
- **Preserves existing phases**: Lexing, parsing, and semantic analysis unchanged
- **Maintains compatibility**: All existing error handling and source mapping works
- **Extends compile options**: New phases added to `CompilePhase` enum
- **Debug support**: IR can be dumped for inspection via `dumpIR()` method

## Usage

### Compilation with IR
The new pipeline is automatically used for all compilation. The `CompileResult` now includes:
```zig
result.ir  // Sea-of-nodes IR for debugging/introspection
```

### Adding New Optimizations
To add a new optimization pass:
1. Implement pass function with signature `fn(*SeaOfNodes, *ErrorCollector) !bool`
2. Add to `PassManager.createStandardPasses()`

### Adding New Code Generation Targets
To add a new backend:
1. Create new codegen module following `codegen_js_ir.zig` pattern
2. Add target to `CompileTarget` enum
3. Add case in `generateCodeFromIr()` method

## Future Enhancements

- **More optimizations**: Loop optimizations, inlining, strength reduction
- **Better type representation**: More sophisticated type system in IR
- **Parallel compilation**: IR structure enables better parallelization
- **Advanced analysis**: Escape analysis, alias analysis, etc.
- **Additional backends**: LLVM IR, WebAssembly, native code generation

## Files Added/Modified

### New Files
- `src/lib/sea_of_nodes_ir.zig` - Core IR data structures
- `src/lib/ast_to_ir.zig` - AST transformation
- `src/lib/ir_optimizer.zig` - Optimization framework
- `src/lib/codegen_js_ir.zig` - JavaScript IR codegen
- `src/lib/codegen_c_ir.zig` - C IR codegen
- `src/test_sea_of_nodes.zig` - IR system tests
- `src/test_ir_main.zig` - Integration test

### Modified Files
- `src/lib/compile_process.zig` - Updated pipeline with IR phases
- `src/root.zig` - Added IR module exports

This implementation provides a solid foundation for advanced compiler optimizations while maintaining the existing compiler's stability and error handling philosophy.