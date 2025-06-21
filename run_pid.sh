#!/bin/sh
# Simple PID wrapper
# Author: luciangreen
# Date: 2025-06-21 22:32:58

# Set the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Check if a file was provided
if [ "$#" -lt 1 ]; then
    echo "Usage: ./run_pid.sh <file.pl> [options]"
    exit 1
fi

# Check if the file exists
if [ ! -f "$1" ]; then
    echo "Error: File not found: $1"
    exit 1
fi

# Get the file and options
FILE="$1"
shift

# Process each option
OPTIONS=""
TAIL=false
PROOF=false
VERBOSE=false

while [ "$#" -gt 0 ]; do
    case "$1" in
        --tail)
            TAIL=true
            ;;
        --proof)
            PROOF=true
            ;;
        --verbose)
            VERBOSE=true
            ;;
        *)
            echo "Warning: Unknown option $1"
            ;;
    esac
    shift
done

# Build the option list for Prolog
OPTIONS_LIST=""
if [ "$TAIL" = "true" ]; then
    OPTIONS_LIST="${OPTIONS_LIST} --tail"
fi
if [ "$PROOF" = "true" ]; then
    OPTIONS_LIST="${OPTIONS_LIST} --proof"
fi
if [ "$VERBOSE" = "true" ]; then
    OPTIONS_LIST="${OPTIONS_LIST} --verbose"
fi

# Show what we're doing
echo "Analyzing $FILE with options:$OPTIONS_LIST"

# Run the analyzer using direct Prolog command
swipl -q -f "$SCRIPT_DIR/pid.pl" -g "main" -- "$FILE" $OPTIONS_LIST