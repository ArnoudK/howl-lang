const std = @import("std");
const ast = @import("ast.zig");
const CompileProcess = @import("compile_process.zig");
const ErrorSystem = @import("error_system.zig");

pub const LspRequest = struct {
    jsonrpc: []const u8 = "2.0",
    id: ?std.json.Value = null,
    method: []const u8,
    params: ?std.json.Value = null,
};

pub const LspResponse = struct {
    jsonrpc: []const u8 = "2.0",
    id: ?std.json.Value = null,
    result: ?std.json.Value = null,
    @"error": ?LspError = null,
};

pub const LspNotification = struct {
    jsonrpc: []const u8 = "2.0",
    method: []const u8,
    params: ?std.json.Value = null,
};

pub const LspError = struct {
    code: i32,
    message: []const u8,
    data: ?std.json.Value = null,
};

pub const Position = struct {
    line: u32,
    character: u32,
};

pub const Range = struct {
    start: Position,
    end: Position,
};

pub const Location = struct {
    uri: []const u8,
    range: Range,
};

pub const TextDocumentIdentifier = struct {
    uri: []const u8,
};

pub const VersionedTextDocumentIdentifier = struct {
    uri: []const u8,
    version: i32,
};

pub const TextDocumentItem = struct {
    uri: []const u8,
    languageId: []const u8,
    version: i32,
    text: []const u8,
};

pub const TextDocumentContentChangeEvent = struct {
    range: ?Range = null,
    rangeLength: ?u32 = null,
    text: []const u8,
};

pub const Diagnostic = struct {
    range: Range,
    severity: ?DiagnosticSeverity = null,
    code: ?std.json.Value = null,
    source: ?[]const u8 = null,
    message: []const u8,
    relatedInformation: ?[]DiagnosticRelatedInformation = null,
};

pub const DiagnosticSeverity = enum(u8) {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4,
};

pub const DiagnosticRelatedInformation = struct {
    location: Location,
    message: []const u8,
};

pub const CompletionItem = struct {
    label: []const u8,
    kind: ?CompletionItemKind = null,
    detail: ?[]const u8 = null,
    documentation: ?[]const u8 = null,
    insertText: ?[]const u8 = null,
};

pub const CompletionItemKind = enum(u8) {
    Text = 1,
    Method = 2,
    Function = 3,
    Constructor = 4,
    Field = 5,
    Variable = 6,
    Class = 7,
    Interface = 8,
    Module = 9,
    Property = 10,
    Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    File = 17,
    Reference = 18,
    Folder = 19,
    EnumMember = 20,
    Constant = 21,
    Struct = 22,
    Event = 23,
    Operator = 24,
    TypeParameter = 25,
};

pub const Hover = struct {
    contents: []const u8,
    range: ?Range = null,
};

pub const InitializeParams = struct {
    processId: ?i32 = null,
    clientInfo: ?struct {
        name: []const u8,
        version: ?[]const u8 = null,
    } = null,
    rootPath: ?[]const u8 = null,
    rootUri: ?[]const u8 = null,
    initializationOptions: ?std.json.Value = null,
    capabilities: ClientCapabilities,
    workspaceFolders: ?[]WorkspaceFolder = null,
};

pub const ClientCapabilities = struct {
    workspace: ?WorkspaceClientCapabilities = null,
    textDocument: ?TextDocumentClientCapabilities = null,
    experimental: ?std.json.Value = null,
};

pub const WorkspaceClientCapabilities = struct {
    applyEdit: ?bool = null,
    workspaceEdit: ?WorkspaceEditClientCapabilities = null,
    didChangeConfiguration: ?DidChangeConfigurationClientCapabilities = null,
    didChangeWatchedFiles: ?DidChangeWatchedFilesClientCapabilities = null,
    symbol: ?WorkspaceSymbolClientCapabilities = null,
    executeCommand: ?ExecuteCommandClientCapabilities = null,
    workspaceFolders: ?bool = null,
    configuration: ?bool = null,
};

pub const WorkspaceEditClientCapabilities = struct {
    documentChanges: ?bool = null,
    resourceOperations: ?[][]const u8 = null,
    failureHandling: ?[]const u8 = null,
};

pub const DidChangeConfigurationClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const DidChangeWatchedFilesClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const WorkspaceSymbolClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const ExecuteCommandClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const TextDocumentClientCapabilities = struct {
    synchronization: ?TextDocumentSyncClientCapabilities = null,
    completion: ?CompletionClientCapabilities = null,
    hover: ?HoverClientCapabilities = null,
    signatureHelp: ?SignatureHelpClientCapabilities = null,
    declaration: ?DeclarationClientCapabilities = null,
    definition: ?DefinitionClientCapabilities = null,
    typeDefinition: ?TypeDefinitionClientCapabilities = null,
    implementation: ?ImplementationClientCapabilities = null,
    references: ?ReferenceClientCapabilities = null,
    documentHighlight: ?DocumentHighlightClientCapabilities = null,
    documentSymbol: ?DocumentSymbolClientCapabilities = null,
    codeAction: ?CodeActionClientCapabilities = null,
    codeLens: ?CodeLensClientCapabilities = null,
    documentLink: ?DocumentLinkClientCapabilities = null,
    colorProvider: ?DocumentColorClientCapabilities = null,
    formatting: ?DocumentFormattingClientCapabilities = null,
    rangeFormatting: ?DocumentRangeFormattingClientCapabilities = null,
    onTypeFormatting: ?DocumentOnTypeFormattingClientCapabilities = null,
    rename: ?RenameClientCapabilities = null,
    publishDiagnostics: ?PublishDiagnosticsClientCapabilities = null,
    foldingRange: ?FoldingRangeClientCapabilities = null,
};

pub const TextDocumentSyncClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    willSave: ?bool = null,
    willSaveWaitUntil: ?bool = null,
    didSave: ?bool = null,
};

pub const CompletionClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    completionItem: ?CompletionItemClientCapabilities = null,
    completionItemKind: ?CompletionItemKindClientCapabilities = null,
    contextSupport: ?bool = null,
};

pub const CompletionItemClientCapabilities = struct {
    snippetSupport: ?bool = null,
    commitCharactersSupport: ?bool = null,
    documentationFormat: ?[][]const u8 = null,
    deprecatedSupport: ?bool = null,
    preselectSupport: ?bool = null,
};

pub const CompletionItemKindClientCapabilities = struct {
    valueSet: ?[]CompletionItemKind = null,
};

pub const HoverClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    contentFormat: ?[][]const u8 = null,
};

pub const SignatureHelpClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    signatureInformation: ?SignatureInformationClientCapabilities = null,
};

pub const SignatureInformationClientCapabilities = struct {
    documentationFormat: ?[][]const u8 = null,
    parameterInformation: ?ParameterInformationClientCapabilities = null,
};

pub const ParameterInformationClientCapabilities = struct {
    labelOffsetSupport: ?bool = null,
};

pub const DeclarationClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    linkSupport: ?bool = null,
};

pub const DefinitionClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    linkSupport: ?bool = null,
};

pub const TypeDefinitionClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    linkSupport: ?bool = null,
};

pub const ImplementationClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    linkSupport: ?bool = null,
};

pub const ReferenceClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const DocumentHighlightClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const DocumentSymbolClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    symbolKind: ?SymbolKindClientCapabilities = null,
    hierarchicalDocumentSymbolSupport: ?bool = null,
};

pub const SymbolKindClientCapabilities = struct {
    valueSet: ?[]SymbolKind = null,
};

pub const SymbolKind = enum(u8) {
    File = 1,
    Module = 2,
    Namespace = 3,
    Package = 4,
    Class = 5,
    Method = 6,
    Property = 7,
    Field = 8,
    Constructor = 9,
    Enum = 10,
    Interface = 11,
    Function = 12,
    Variable = 13,
    Constant = 14,
    String = 15,
    Number = 16,
    Boolean = 17,
    Array = 18,
    Object = 19,
    Key = 20,
    Null = 21,
    EnumMember = 22,
    Struct = 23,
    Event = 24,
    Operator = 25,
    TypeParameter = 26,
};

pub const CodeActionClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    codeActionLiteralSupport: ?CodeActionLiteralSupportClientCapabilities = null,
};

pub const CodeActionLiteralSupportClientCapabilities = struct {
    codeActionKind: CodeActionKindClientCapabilities,
};

pub const CodeActionKindClientCapabilities = struct {
    valueSet: [][]const u8,
};

pub const CodeLensClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const DocumentLinkClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    tooltipSupport: ?bool = null,
};

pub const DocumentColorClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const DocumentFormattingClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const DocumentRangeFormattingClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const DocumentOnTypeFormattingClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
};

pub const RenameClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    prepareSupport: ?bool = null,
};

pub const PublishDiagnosticsClientCapabilities = struct {
    relatedInformation: ?bool = null,
    tagSupport: ?PublishDiagnosticsTagSupportClientCapabilities = null,
};

pub const PublishDiagnosticsTagSupportClientCapabilities = struct {
    valueSet: []DiagnosticTag,
};

pub const DiagnosticTag = enum(u8) {
    Unnecessary = 1,
    Deprecated = 2,
};

pub const FoldingRangeClientCapabilities = struct {
    dynamicRegistration: ?bool = null,
    rangeLimit: ?u32 = null,
    lineFoldingOnly: ?bool = null,
};

pub const WorkspaceFolder = struct {
    uri: []const u8,
    name: []const u8,
};

pub const InitializeResult = struct {
    capabilities: ServerCapabilities,
    serverInfo: ?struct {
        name: []const u8,
        version: ?[]const u8 = null,
    } = null,
};

pub const ServerCapabilities = struct {
    textDocumentSync: ?TextDocumentSyncOptions = null,
    completionProvider: ?CompletionOptions = null,
    hoverProvider: ?bool = null,
    signatureHelpProvider: ?SignatureHelpOptions = null,
    declarationProvider: ?bool = null,
    definitionProvider: ?bool = null,
    typeDefinitionProvider: ?bool = null,
    implementationProvider: ?bool = null,
    referencesProvider: ?bool = null,
    documentHighlightProvider: ?bool = null,
    documentSymbolProvider: ?bool = null,
    codeActionProvider: ?bool = null,
    codeLensProvider: ?CodeLensOptions = null,
    documentLinkProvider: ?DocumentLinkOptions = null,
    colorProvider: ?bool = null,
    documentFormattingProvider: ?bool = null,
    documentRangeFormattingProvider: ?bool = null,
    documentOnTypeFormattingProvider: ?DocumentOnTypeFormattingOptions = null,
    renameProvider: ?bool = null,
    foldingRangeProvider: ?bool = null,
    executeCommandProvider: ?ExecuteCommandOptions = null,
    selectionRangeProvider: ?bool = null,
    workspaceSymbolProvider: ?bool = null,
    workspace: ?WorkspaceServerCapabilities = null,
    experimental: ?std.json.Value = null,
};

pub const TextDocumentSyncOptions = struct {
    openClose: ?bool = null,
    change: ?TextDocumentSyncKind = null,
    willSave: ?bool = null,
    willSaveWaitUntil: ?bool = null,
    save: ?SaveOptions = null,
};

pub const TextDocumentSyncKind = enum(u8) {
    None = 0,
    Full = 1,
    Incremental = 2,
};

pub const SaveOptions = struct {
    includeText: ?bool = null,
};

pub const CompletionOptions = struct {
    triggerCharacters: ?[][]const u8 = null,
    allCommitCharacters: ?[][]const u8 = null,
    resolveProvider: ?bool = null,
};

pub const SignatureHelpOptions = struct {
    triggerCharacters: ?[][]const u8 = null,
    retriggerCharacters: ?[][]const u8 = null,
};

pub const CodeLensOptions = struct {
    resolveProvider: ?bool = null,
};

pub const DocumentLinkOptions = struct {
    resolveProvider: ?bool = null,
};

pub const DocumentOnTypeFormattingOptions = struct {
    firstTriggerCharacter: []const u8,
    moreTriggerCharacter: ?[][]const u8 = null,
};

pub const ExecuteCommandOptions = struct {
    commands: [][]const u8,
};

pub const WorkspaceServerCapabilities = struct {
    workspaceFolders: ?WorkspaceFoldersServerCapabilities = null,
};

pub const WorkspaceFoldersServerCapabilities = struct {
    supported: ?bool = null,
    changeNotifications: ?std.json.Value = null,
};
