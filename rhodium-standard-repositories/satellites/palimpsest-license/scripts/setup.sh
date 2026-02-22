#!/bin/bash
#
# Simple setup script for the Palimpsest project.

set -e # Exit immediately if a command exits with a non-zero status.

echo "🔎 Checking for dependencies..."

# Check for Node.js and npm
if ! command -v npm &> /dev/null
then
    echo "❌ Node.js/npm could not be found. Please install it to continue."
    exit 1
fi

# Check for Python and pip
if ! command -v pip &> /dev/null
then
    echo "❌ Python/pip could not be found. Please install it to continue."
    exit 1
fi

echo "✅ All dependencies found."
echo "🚀 Installing project packages..."

# Use Makefile to install everything
make install

echo "🎉 Setup is complete! You are ready to contribute."