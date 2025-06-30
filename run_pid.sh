#!/bin/sh
# Simple PID wrapper
# Author: luciangreen
# Date: 2025-06-30 04:05:43

# Get absolute path to script directory without using getcwd
SCRIPT_DIR=$(dirname "$0")
SCRIPT_DIR=$(cd "$SCRIPT_DIR" 2>/dev/null && pwd || echo "$SCRIPT_DIR")
ABS_SCRIPT_PATH=$(cd "$SCRIPT_DIR" 2>/dev/null && echo "$PWD/$(basename "$0")" || echo "$0")

# Check if a file was provided
if [ "$#" -lt 1 ]; then
    echo "Usage: ./run_pid.sh <file.pl> [options]"
    exit 1
fi

# Get the file path
FILE="$1"
if [ ! -f "$FILE" ]; then
    echo "Error: File not found: $FILE"
    exit 1
fi

# Convert to absolute path
FILE=$(cd "$(dirname "$FILE")" 2>/dev/null && echo "$PWD/$(basename "$FILE")" || echo "$FILE")

# Process and collect options
shift
OPTIONS=""

# Parse command line options
while [ $# -gt 0 ]; do
    case "$1" in
        --tail)
            OPTIONS="$OPTIONS --tail"
            ;;
        --proof)
            OPTIONS="$OPTIONS --proof"
            ;;
        --verbose)
            OPTIONS="$OPTIONS --verbose"
            ;;
        *)
            echo "Warning: Unknown option $1"
            ;;
    esac
    shift
done

# Show what we're doing
echo "Analyzing $FILE with options:$OPTIONS"

# Create a temporary directory that's guaranteed to be accessible
TEMP_DIR=$(mktemp -d)
if [ ! -d "$TEMP_DIR" ]; then
    echo "Error: Failed to create temporary directory"
    exit 1
fi

# Change to the temporary directory for execution
cd "$TEMP_DIR" || { echo "Error: Failed to change to temporary directory"; exit 1; }

# Run the analyzer using direct Prolog command from the temp dir
swipl -q -f "$SCRIPT_DIR/pid.pl" -g "main" -- "$FILE" $OPTIONS

# Store the exit status
STATUS=$?

# Clean up temporary directory
rm -rf "$TEMP_DIR"

# Return the exit status of swipl
exit $STATUS