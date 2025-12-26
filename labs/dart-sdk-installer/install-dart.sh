#!/bin/bash

# Dart SDK installer for macOS
# This script fetches the latest stable Dart SDK version from homebrew-dart Formula
# and installs it to ~/.local/share/mise/installs/dart

set -e

# Configuration
HOMEBREW_FORMULA_URL="https://raw.githubusercontent.com/dart-lang/homebrew-dart/main/Formula/dart.rb"
DART_INSTALL_BASE="$HOME/.local/share/mise/installs/dart"
TEMP_DIR=$(mktemp -d)

echo "🚀 Dart SDK installer for macOS"
echo ""

# Cleanup on exit
cleanup() {
    if [ -d "$TEMP_DIR" ]; then
        echo "Cleaning up temporary files..."
        rm -rf "$TEMP_DIR"
    fi
}
trap cleanup EXIT

# Fetch the Formula file
echo "📥 Fetching latest Dart SDK info from Homebrew Formula..."
FORMULA_CONTENT=$(curl -s "$HOMEBREW_FORMULA_URL")

echo "Formula content (first 30 lines):"
echo "$FORMULA_CONTENT" | head -30
echo ""

# Detect CPU architecture
if [[ $(uname -m) == "arm64" ]]; then
    ARCH="arm64"
    echo "Detected architecture: ARM64 (Apple Silicon)"
else
    ARCH="x64"
    echo "Detected architecture: x64 (Intel)"
fi
echo ""

# Extract the stable release URL for macOS
echo "🔍 Extracting download URL for macOS $ARCH..."

# Find the stable release section (after "conflicts_with" and before any "head" section)
# Extract URL for macOS with the detected architecture
DOWNLOAD_URL=$(echo "$FORMULA_CONTENT" | \
    awk '/conflicts_with.*dart-beta/,/^  def install/' | \
    grep "dartsdk-macos-${ARCH}-release.zip" | \
    grep -o 'https://[^"]*' | \
    head -1)

if [ -z "$DOWNLOAD_URL" ]; then
    echo "❌ Failed to extract download URL from Formula"
    echo "Trying alternative extraction method..."
    
    # Alternative: look for any macos URL in the stable section
    DOWNLOAD_URL=$(echo "$FORMULA_CONTENT" | \
        grep "storage.googleapis.com" | \
        grep "stable/release" | \
        grep "macos-${ARCH}" | \
        grep -o 'https://[^"]*' | \
        head -1)
fi

if [ -z "$DOWNLOAD_URL" ]; then
    echo "❌ Could not find download URL"
    exit 1
fi

echo "Download URL: $DOWNLOAD_URL"
echo ""

# Extract version from URL
DART_VERSION=$(echo "$DOWNLOAD_URL" | grep -o '/release/[^/]*/' | sed 's|/release/||' | sed 's|/||')

if [ -z "$DART_VERSION" ]; then
    echo "❌ Failed to extract version from URL"
    exit 1
fi

echo "Dart version: $DART_VERSION"
DART_INSTALL_DIR="$DART_INSTALL_BASE/$DART_VERSION"
echo "Install directory: $DART_INSTALL_DIR"
echo ""

# Check if already installed
if [ -d "$DART_INSTALL_DIR" ] && [ -f "$DART_INSTALL_DIR/bin/dart" ]; then
    echo "⚠️  Dart $DART_VERSION is already installed at $DART_INSTALL_DIR"
    echo ""
    read -p "Do you want to reinstall? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Installation cancelled."
        exit 0
    fi
    echo "Removing existing installation..."
    rm -rf "$DART_INSTALL_DIR"
fi

# Create installation directory
echo "📁 Creating installation directory..."
mkdir -p "$DART_INSTALL_DIR"

# Download Dart SDK
cd "$TEMP_DIR"
SDK_FILENAME="dartsdk-macos-${ARCH}-release.zip"

echo "⬇️  Downloading Dart SDK..."
echo "URL: $DOWNLOAD_URL"
echo ""

if ! curl -f -L -o "$SDK_FILENAME" "$DOWNLOAD_URL"; then
    echo "❌ Failed to download Dart SDK"
    exit 1
fi

# Check if file was actually downloaded
if [ ! -f "$SDK_FILENAME" ] || [ ! -s "$SDK_FILENAME" ]; then
    echo "❌ Download failed - file is empty or doesn't exist"
    ls -lah "$TEMP_DIR"
    exit 1
fi

echo "✅ Download completed: $SDK_FILENAME"
echo "File size: $(du -h "$SDK_FILENAME" | cut -f1)"
echo ""

# Extract the archive
echo "📦 Extracting archive..."
if ! unzip -q "$SDK_FILENAME"; then
    echo "❌ Failed to extract archive"
    exit 1
fi

# Check if extraction was successful
if [ ! -d "dart-sdk" ]; then
    echo "❌ Failed to extract dart-sdk from archive"
    echo "Contents of $TEMP_DIR:"
    ls -la "$TEMP_DIR"
    exit 1
fi

echo "✅ Extraction completed"
echo ""

# Move contents to installation directory
echo "🔄 Installing to $DART_INSTALL_DIR..."
cp -r dart-sdk/* "$DART_INSTALL_DIR/"

# Verify installation
if [ ! -f "$DART_INSTALL_DIR/bin/dart" ]; then
    echo "❌ Installation verification failed - dart binary not found"
    exit 1
fi

echo "✅ Installation completed successfully!"
echo ""
echo "📍 Dart SDK $DART_VERSION installed at: $DART_INSTALL_DIR"
echo ""
echo "To use this version with mise, run:"
echo "  mise use --global dart@$DART_VERSION"
echo ""
echo "Or manually add to your PATH:"
echo "  export PATH=\"\$PATH:$DART_INSTALL_DIR/bin\""
echo ""
echo "Verify the installation:"
echo "  $DART_INSTALL_DIR/bin/dart --version"
