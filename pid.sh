#!/bin/sh
# Shell script wrapper for Prolog Inductive Debugger
# Author: luciangreen
# Date: 2025-06-21 21:31:10

# Find the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Check if at least one argument (file) is provided
if [ "$#" -lt 1 ]; then
    # No arguments provided, show usage
    echo "Prolog Inductive Debugger (PID) - Static Analysis Tool"
    echo "Author: luciangreen"
    echo "Date: 2025-06-21 21:31:10"
    echo
    echo "Usage: ./pid.sh <file.pl> [options]"
    echo
    echo "Options:"
    echo "  --tail     Check for tail recursion"
    echo "  --proof    Generate proof traces"
    echo "  --verbose  Verbose output"
    echo
    echo "Example: ./pid.sh examples/reverse.pl --tail --proof"
    echo
    exit 1
fi

# Check if the file exists
if [ ! -f "$1" ]; then
    echo "Error: File '$1' not found."
    exit 1
fi

# Extract the arguments and convert spaces if any
FILE="$1"
shift
OPTIONS=""
for arg in "$@"; do
    OPTIONS="$OPTIONS \"$arg\""
done

# Run PID with all arguments - using the simpler approach
echo "Running: swipl -q -f $SCRIPT_DIR/pid_check.pl -g \"analyze('$FILE',$OPTIONS)\" -t halt"
swipl -q -f "$SCRIPT_DIR/pid_check.pl" -g "analyze('$FILE',$OPTIONS)" -t halt
exit $?