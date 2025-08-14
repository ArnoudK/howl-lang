import * as vscode from 'vscode';
import * as path from 'path';
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

    // Register commands
    const restartCommand = vscode.commands.registerCommand('howl.restartLanguageServer', async () => {
        await restartLanguageServer(context);
        vscode.window.showInformationMessage('Howl Language Server restarted');
    });

    const showOutputCommand = vscode.commands.registerCommand('howl.showOutputChannel', () => {
        outputChannel.show();
    });

    context.subscriptions.push(restartCommand, showOutputCommand);

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

    const serverPath = config.get<string>('serverPath', 'howl_lsp');
    const traceLevel = config.get<string>('trace.server', 'off');

    outputChannel.appendLine(`Starting Howl Language Server...`);
    outputChannel.appendLine(`Server path: ${serverPath}`);
    outputChannel.appendLine(`Trace level: ${traceLevel}`);

    // Try to find the server executable
    let serverCommand = serverPath;
    if (!path.isAbsolute(serverPath)) {
        // If it's not an absolute path, try to find it relative to the workspace
        const workspaceFolders = vscode.workspace.workspaceFolders;
        if (workspaceFolders && workspaceFolders.length > 0) {
            const workspaceRoot = workspaceFolders[0].uri.fsPath;
            const possiblePaths = [
                path.join(workspaceRoot, 'zig-out', 'bin', 'howl_lsp'),
                path.join(workspaceRoot, 'zig-out', 'bin', 'howl_lsp.exe'),
                path.join(workspaceRoot, serverPath),
                serverPath // fallback to PATH lookup
            ];

            // Check which path might exist
            const fs = require('fs');
            for (const testPath of possiblePaths) {
                try {
                    if (fs.existsSync(testPath)) {
                        serverCommand = testPath;
                        outputChannel.appendLine(`Found server at: ${serverCommand}`);
                        break;
                    }
                } catch (error) {
                    // Continue to next path
                }
            }
        }
    }

    outputChannel.appendLine(`Using server command: ${serverCommand}`);

    // Server options
    const serverOptions: ServerOptions = {
        command: serverCommand,
        args: [],
        options: {
            env: {
                ...process.env,
            }
        }
    };

    // Client options
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
            outputChannel.appendLine('1. Make sure howl_lsp is built: run "zig build" in your Howl project');
            outputChannel.appendLine('2. Check the server path in settings (Ctrl+, then search "howl")');
            outputChannel.appendLine('3. Verify the executable exists and is executable');
            outputChannel.appendLine('4. Try setting an absolute path to the howl_lsp executable');
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