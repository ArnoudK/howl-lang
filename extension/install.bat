@echo off
setlocal enabledelayedexpansion

echo 🐺 Installing Howl Language Support for VS Code...

REM Set the VS Code extensions directory for Windows
set "VSCODE_EXTENSIONS_DIR=%USERPROFILE%\.vscode\extensions"
set "EXTENSION_DIR=%VSCODE_EXTENSIONS_DIR%\howl-language-support-0.1.0"

echo 📁 Installing to: %EXTENSION_DIR%

REM Create the extensions directory if it doesn't exist
if not exist "%VSCODE_EXTENSIONS_DIR%" (
    mkdir "%VSCODE_EXTENSIONS_DIR%"
)

REM Remove existing installation
if exist "%EXTENSION_DIR%" (
    echo 🗑️  Removing existing installation...
    rmdir /s /q "%EXTENSION_DIR%"
)

REM Copy extension files
echo 📋 Copying extension files...
xcopy "%~dp0" "%EXTENSION_DIR%\" /e /i /y /q

REM Clean up development files
cd /d "%EXTENSION_DIR%"
if exist "install.bat" del "install.bat"
if exist "install.sh" del "install.sh"
if exist "node_modules\.cache" rmdir /s /q "node_modules\.cache"
if exist ".git" rmdir /s /q ".git"

echo ✅ Installation complete!
echo.
echo 📖 Next steps:
echo 1. Restart VS Code
echo 2. Open a .howl file
echo 3. The extension should activate automatically
echo.
echo 🔧 Configuration:
echo - Set 'howl.lsp.serverPath' to point to your howl_lsp.exe
echo - Check 'Howl: Show Output Channel' if you encounter issues
echo.
echo 🎉 Happy coding with Howl!

pause