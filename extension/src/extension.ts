import * as vscode from 'vscode';
import * as path from 'path';
import * as child_process from 'child_process';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel;

export function activate(context: vscode.ExtensionContext) {
    console.log('Howl Language Support extension is now active!');

    // Create output channel
    outputChannel = vscode.window.createOutputChannel('Howl Language Server');
    context.subscriptions.push(outputChannel);

    // Start the language server
    startLanguageServer(context);

    // Register formatting providers
    registerFormattingProviders(context);

    // Register format-on-save handler
    registerFormatOnSave(context);

    // Register commands
    const restartCommand = vscode.commands.registerCommand('howl.restartLanguageServer', async () => {
        await restartLanguageServer(context);
        vscode.window.showInformationMessage('Howl Language Server restarted');
    });

    const showOutputCommand = vscode.commands.registerCommand('howl.showOutputChannel', () => {
        outputChannel.show();
    });

    const buildCurrentFileCommand = vscode.commands.registerCommand('howl.buildCurrentFile', async () => {
        const activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor || activeEditor.document.languageId !== 'howl') {
            vscode.window.showWarningMessage('No Howl file is currently active');
            return;
        }

        const filePath = activeEditor.document.fileName;
        const config = vscode.workspace.getConfiguration('howl.lsp');
        const howlPath = config.get<string>('serverPath', 'howl');
        
        const terminal = vscode.window.createTerminal('Howl Build');
        terminal.sendText(`${howlPath} build "${filePath}" -tjs`);
        terminal.show();
    });

    const runCurrentFileCommand = vscode.commands.registerCommand('howl.runCurrentFile', async () => {
        const activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor || activeEditor.document.languageId !== 'howl') {
            vscode.window.showWarningMessage('No Howl file is currently active');
            return;
        }

        const filePath = activeEditor.document.fileName;
        const config = vscode.workspace.getConfiguration('howl.lsp');
        const howlPath = config.get<string>('serverPath', 'howl');
        
        const terminal = vscode.window.createTerminal('Howl Run');
        terminal.sendText(`${howlPath} run "${filePath}" -tjs`);
        terminal.show();
    });

    const formatDocumentCommand = vscode.commands.registerCommand('howl.formatDocument', async () => {
        const activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor || activeEditor.document.languageId !== 'howl') {
            vscode.window.showWarningMessage('No Howl file is currently active');
            return;
        }

        await formatDocument(activeEditor.document, null);
    });

    const formatSelectionCommand = vscode.commands.registerCommand('howl.formatSelection', async () => {
        const activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor || activeEditor.document.languageId !== 'howl') {
            vscode.window.showWarningMessage('No Howl file is currently active');
            return;
        }

        const selection = activeEditor.selection;
        if (selection.isEmpty) {
            vscode.window.showWarningMessage('No text selected');
            return;
        }

        const range = new vscode.Range(selection.start, selection.end);
        await formatDocument(activeEditor.document, range);
    });

    context.subscriptions.push(restartCommand, showOutputCommand, buildCurrentFileCommand, runCurrentFileCommand, formatDocumentCommand, formatSelectionCommand);

    // Register configuration change handler
    const configChangeHandler = vscode.workspace.onDidChangeConfiguration((event) => {
        if (event.affectsConfiguration('howl.lsp')) {
            vscode.window.showInformationMessage('Howl LSP configuration changed. Restart the language server to apply changes.', 'Restart')
                .then((selection) => {
                    if (selection === 'Restart') {
                        vscode.commands.executeCommand('howl.restartLanguageServer');
                    }
                });
        }
    });

    context.subscriptions.push(configChangeHandler);
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

async function startLanguageServer(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('howl.lsp');
    
    // Check if LSP is enabled
    if (!config.get<boolean>('enabled', true)) {
        outputChannel.appendLine('Howl Language Server is disabled in settings');
        return;
    }

    const serverPath = config.get<string>('serverPath', 'howl');
    const traceLevel = config.get<string>('trace.server', 'off');

    outputChannel.appendLine(`Starting Howl Language Server...`);
    outputChannel.appendLine(`Server path: ${serverPath}`);
    outputChannel.appendLine(`Trace level: ${traceLevel}`);

    // Try to find the howl executable
    let serverCommand = serverPath;
    if (!path.isAbsolute(serverPath)) {
        // If it's not an absolute path, try to find it relative to the workspace
        const workspaceFolders = vscode.workspace.workspaceFolders;
        if (workspaceFolders && workspaceFolders.length > 0) {
            const workspaceRoot = workspaceFolders[0].uri.fsPath;
            const possiblePaths = [
                path.join(workspaceRoot, 'zig-out', 'bin', 'howl'),
                path.join(workspaceRoot, 'zig-out', 'bin', 'howl.exe'),
                path.join(workspaceRoot, serverPath),
                serverPath // fallback to PATH lookup
            ];

            // Check which path might exist
            const fs = require('fs');
            for (const testPath of possiblePaths) {
                try {
                    if (fs.existsSync(testPath)) {
                        serverCommand = testPath;
                        outputChannel.appendLine(`Found howl executable at: ${serverCommand}`);
                        break;
                    }
                } catch (error) {
                    // Continue to next path
                }
            }
        }
    }

    outputChannel.appendLine(`Using server command: ${serverCommand} lsp`);

    // Server options - now using "howl lsp" subcommand
    const serverOptions: ServerOptions = {
        command: serverCommand,
        args: ['lsp'],
        options: {
            env: {
                ...process.env,
            }
        }
    };

    // Client options with enhanced capabilities
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'howl' },
            { scheme: 'untitled', language: 'howl' }
        ],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.howl')
        },
        outputChannel: outputChannel,
        traceOutputChannel: outputChannel,
        revealOutputChannelOn: 4, // Never automatically reveal
        initializationOptions: {
            // Enhanced initialization options for better autocomplete
            provideFormatter: true,
            provideCodeActions: true,
            provideHover: true,
            provideCompletion: true,
            completionTriggerCharacters: ['.', '@', '{', '}'],
            // Formatter options
            formatterOptions: getFormattingOptions(),
        },
    };

    // Create and start the language client
    client = new LanguageClient(
        'howlLanguageServer',
        'Howl Language Server',
        serverOptions,
        clientOptions
    );

    // Set trace level
    if (traceLevel !== 'off') {
        client.setTrace(traceLevel as any);
    }

    try {
        // Start the client and server
        await client.start();
        outputChannel.appendLine('Howl Language Server started successfully');

        // Handle server errors
        client.onDidChangeState((event) => {
            outputChannel.appendLine(`Language server state changed: ${event.oldState} -> ${event.newState}`);
            
            if (event.newState === 3) { // Stopped
                outputChannel.appendLine('Language server stopped');
            } else if (event.newState === 2) { // Running
                outputChannel.appendLine('Language server is now running');
            }
        });

    } catch (error) {
        outputChannel.appendLine(`Failed to start Howl Language Server: ${error}`);
        
        // Check if it's a "command not found" type error
        const errorMessage = error instanceof Error ? error.message : String(error);
        if (errorMessage.includes('ENOENT') || errorMessage.includes('not found')) {
            outputChannel.appendLine('');
            outputChannel.appendLine('🔍 Troubleshooting tips:');
            outputChannel.appendLine('1. Make sure howl is built: run "zig build" in your Howl project');
            outputChannel.appendLine('2. Check the server path in settings (Ctrl+, then search "howl")');
            outputChannel.appendLine('3. Verify the howl executable exists and is executable');
            outputChannel.appendLine('4. Try setting an absolute path to the howl executable');
            outputChannel.appendLine('5. The LSP server is now integrated: use "howl lsp" instead of separate howl_lsp');
        }
        
        vscode.window.showErrorMessage(`Failed to start Howl Language Server: ${errorMessage}`, 'Show Output', 'Open Settings')
            .then((selection) => {
                if (selection === 'Show Output') {
                    outputChannel.show();
                } else if (selection === 'Open Settings') {
                    vscode.commands.executeCommand('workbench.action.openSettings', 'howl.lsp');
                }
            });
    }
}

async function restartLanguageServer(context: vscode.ExtensionContext) {
    outputChannel.appendLine('Restarting Howl Language Server...');
    
    if (client) {
        await client.stop();
        client = undefined;
    }
    
    await startLanguageServer(context);
}

function registerFormattingProviders(context: vscode.ExtensionContext) {
    // Register document formatting provider
    const documentFormattingProvider = vscode.languages.registerDocumentFormattingEditProvider(
        'howl',
        {
            provideDocumentFormattingEdits(document: vscode.TextDocument): vscode.ProviderResult<vscode.TextEdit[]> {
                return formatDocumentWithLSP(document, null);
            }
        }
    );

    // Register document range formatting provider  
    const documentRangeFormattingProvider = vscode.languages.registerDocumentRangeFormattingEditProvider(
        'howl',
        {
            provideDocumentRangeFormattingEdits(document: vscode.TextDocument, range: vscode.Range): vscode.ProviderResult<vscode.TextEdit[]> {
                return formatDocumentWithLSP(document, range);
            }
        }
    );

    context.subscriptions.push(documentFormattingProvider, documentRangeFormattingProvider);
}

function registerFormatOnSave(context: vscode.ExtensionContext) {
    const formatOnSaveDisposable = vscode.workspace.onWillSaveTextDocument(async (event) => {
        // Check if this is a Howl file and format-on-save is enabled
        const document = event.document;
        if (document.languageId !== 'howl') {
            return;
        }

        const config = vscode.workspace.getConfiguration('howl.formatter');
        const editorConfig = vscode.workspace.getConfiguration('editor', document.uri);
        
        // Check both our specific setting and the general editor formatOnSave setting
        const howlFormatOnSave = config.get<boolean>('formatOnSave', false);
        const editorFormatOnSave = editorConfig.get<boolean>('formatOnSave', false);
        
        if (!howlFormatOnSave && !editorFormatOnSave) {
            return;
        }

        // Wait for formatting to complete
        event.waitUntil(
            (async () => {
                try {
                    const edits = await formatDocumentWithLSP(document, null);
                    if (edits.length > 0) {
                        const edit = new vscode.WorkspaceEdit();
                        edit.set(document.uri, edits);
                        await vscode.workspace.applyEdit(edit);
                    }
                } catch (error) {
                    outputChannel.appendLine(`Format on save failed: ${error}`);
                    // Don't show error messages for format-on-save to avoid interrupting workflow
                }
            })()
        );
    });

    context.subscriptions.push(formatOnSaveDisposable);
}

async function formatDocumentWithLSP(document: vscode.TextDocument, range: vscode.Range | null): Promise<vscode.TextEdit[]> {
    try {
        // If LSP client is available and supports formatting, use it
        if (client && client.initializeResult?.capabilities?.documentFormattingProvider) {
            if (range) {
                // Use LSP range formatting
                const edits = await client.sendRequest('textDocument/rangeFormatting', {
                    textDocument: { uri: document.uri.toString() },
                    range: {
                        start: { line: range.start.line, character: range.start.character },
                        end: { line: range.end.line, character: range.end.character }
                    },
                    options: getFormattingOptions()
                });
                return (edits && Array.isArray(edits)) ? edits.map((edit: any) => new vscode.TextEdit(
                    new vscode.Range(edit.range.start.line, edit.range.start.character, edit.range.end.line, edit.range.end.character),
                    edit.newText
                )) : [];
            } else {
                // Use LSP document formatting
                const edits = await client.sendRequest('textDocument/formatting', {
                    textDocument: { uri: document.uri.toString() },
                    options: getFormattingOptions()
                });
                return (edits && Array.isArray(edits)) ? edits.map((edit: any) => new vscode.TextEdit(
                    new vscode.Range(edit.range.start.line, edit.range.start.character, edit.range.end.line, edit.range.end.character),
                    edit.newText
                )) : [];
            }
        } else {
            // Fallback to direct howl fmt command
            return await formatDocumentWithCLI(document, range);
        }
    } catch (error) {
        console.error('LSP formatting failed, falling back to CLI:', error);
        return await formatDocumentWithCLI(document, range);
    }
}

async function formatDocumentWithCLI(document: vscode.TextDocument, range: vscode.Range | null): Promise<vscode.TextEdit[]> {
    const config = vscode.workspace.getConfiguration('howl');
    
    // Check if formatter is enabled
    if (!config.get<boolean>('formatter.enable', true)) {
        return [];
    }

    try {
        // Get howl executable path
        const lspConfig = vscode.workspace.getConfiguration('howl.lsp');
        const howlPath = lspConfig.get<string>('serverPath', 'howl');
        
        // Use simple fmt command without configuration arguments
        const args = ['fmt'];

        // Create temporary file
        const tempDir = require('os').tmpdir();
        const tempFile = path.join(tempDir, `howl_format_${Date.now()}.howl`);
        
        // Write document content to temp file
        const fs = require('fs');
        let content = document.getText();
        if (range) {
            content = document.getText(range);
        }
        
        fs.writeFileSync(tempFile, content);
        
        try {
            // Run format command
            args.push(tempFile);
            
            const result = child_process.execFileSync(howlPath, args, {
                encoding: 'utf8',
                timeout: 30000 // 30 second timeout
            });

            // Read formatted content
            const formattedContent = fs.readFileSync(tempFile, 'utf8');
            
            // Clean up temp file
            fs.unlinkSync(tempFile);
            
            // Create text edit
            if (range) {
                return [new vscode.TextEdit(range, formattedContent)];
            } else {
                const fullRange = new vscode.Range(
                    document.positionAt(0),
                    document.positionAt(document.getText().length)
                );
                return [new vscode.TextEdit(fullRange, formattedContent)];
            }
            
        } catch (formatError) {
            // Clean up temp file
            if (fs.existsSync(tempFile)) {
                fs.unlinkSync(tempFile);
            }
            throw formatError;
        }
        
    } catch (error) {
        const errorMessage = error instanceof Error ? error.message : String(error);
        outputChannel.appendLine(`Formatting failed: ${errorMessage}`);
        vscode.window.showErrorMessage(`Failed to format Howl code: ${errorMessage}`);
        return [];
    }
}

async function formatDocument(document: vscode.TextDocument, range: vscode.Range | null): Promise<void> {
    try {
        const edits = await formatDocumentWithLSP(document, range);
        if (edits.length > 0) {
            const edit = new vscode.WorkspaceEdit();
            edit.set(document.uri, edits);
            await vscode.workspace.applyEdit(edit);
        }
    } catch (error) {
        const errorMessage = error instanceof Error ? error.message : String(error);
        vscode.window.showErrorMessage(`Failed to format document: ${errorMessage}`);
    }
}

function getFormattingOptions(): any {
    const editorConfig = vscode.workspace.getConfiguration('editor');
    
    return {
        tabSize: editorConfig.get<number>('tabSize', 4),
        insertSpaces: editorConfig.get<boolean>('insertSpaces', true),
        trimTrailingWhitespace: editorConfig.get<boolean>('trimAutoWhitespace', true),
        insertFinalNewline: editorConfig.get<boolean>('insertFinalNewline', false)
    };
}