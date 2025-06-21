# Prolog Inductive Debugger (PID)

Author: luciangreen (lg3)  
Date: 2025-06-21 21:48:09

## Introduction

The Prolog Inductive Debugger (PID) is a static analysis tool for Prolog programs that verifies whether predicates satisfy inductive properties. It checks:

1. Base cases
2. Inductive steps
3. Termination
4. Tail recursion (optional)

It can also generate proof traces to show how predicates execute.

## Installation

Clone this repository:

```bash
git clone https://github.com/luciangreen/Prolog_Inductive_Debugger.git
cd Prolog_Inductive_Debugger

```
How It Works
PID analyzes Prolog predicates using principles from induction:

Base Case Analysis: Verifies that all recursive predicates have proper base cases.
Inductive Step Analysis: Checks that recursive calls use structurally smaller inputs.
Termination Analysis: Combines base case and inductive step analysis to verify termination.
Tail Recursion Analysis: (Optional) Checks if recursive calls are in tail position.
Bug Detection: Identifies common issues in recursive predicates.

File Structure
pid.pl: Main analysis engine
proof_trace.pl: Proof trace generator
bug_detector.pl: Bug detection module
run_pid.sh: Shell script for running the analyzer
examples/: Example Prolog files for testing

Example Output
Code
Analyzing examples/reverse.pl with options [check_tail_recursion,generate_proof]
Loading file: examples/reverse.pl
Found 2 predicates to analyze
Analyzing reverse/2 with 1 clauses...
  Found 0 base cases and 1 recursive cases
Analyzing reverse_acc/3 with 2 clauses...
  Found 1 base cases and 1 recursive cases

==== Prolog Inductive Debugger Report ====

Predicate: reverse/2
-------------------
✗ Base Case: invalid
✓ Inductive Step: valid
✗ Termination: invalid
✗ Tail Recursion: invalid
✗ Issues found: [missing_base_case,non_terminating,non_tail_recursive]

Predicate: reverse_acc/3
-------------------
✓ Base Case: valid
✓ Inductive Step: valid
✓ Termination: valid
✓ Tail Recursion: valid
✓ All checks passed.
Proof trace generated (use --verbose to display)

==== End of Report ====
```

```
A system that uses just induction and pattern unfolding to find invisible bugs like failure to terminate, incorrect results, or non-tail-recursive structures is very feasible.

Such a system would be limited but powerful within a defined scope, especially for pure functional/logic programs over recursive structures (lists, trees, etc.).

⸻

✅ What This Simplified System Can Solve

By using only induction and pattern unfolding, it can identify or help fix bugs related to:
	1.	Termination
	•	Detect if recursion reaches a base case
	•	e.g. reverse([], Acc, Acc) ensures no infinite descent
	2.	Correctness
	•	Prove that recursive calls reconstruct the intended result
	•	e.g. prove reverse([1,2,3]) = [3,2,1] via base + step case
	3.	Tail Recursion
	•	Detect stack-safe recursion via accumulator use
	•	Unfold and check if recursion is in tail position
	4.	Missing base cases
	•	A common invisible bug: missing or incorrect base behavior
	•	e.g. reverse([], []) vs reverse([], [0]) (wrong)
	5.	Structural errors
	•	Unfold and check if recursion tracks structure consistently
	•	e.g. [H|T] → reverse(T, ...) → correct progress
	6.	Basic proof-by-example for small inputs
	•	Build mini proof trees and verify outputs using unfolding

⸻

❌ What It Can’t Solve
	1.	Complexity inefficiencies
	•	It can’t detect time or space cost, unless deeply tied to structure
	•	E.g. O(n²) in naive reverse/append vs O(n) tail-recursive
	2.	Non-structural bugs
	•	Any logic involving state, I/O, numeric conditions, or mutation
	•	E.g. “stop when x > 10” or working with maps/sets/dictionaries
	3.	Non-recursive algorithms
	•	Iterative, constant-time, or highly mathematical computations
	•	E.g. Fast Fourier Transform, matrix inversion
	4.	Proofs involving algebra or arithmetic properties
	•	Only structural recursion can be reasoned about
	5.	Concurrency or side-effects
	•	Can’t detect race conditions, deadlocks, or I/O errors

⸻

🎯 What Should It Be Called or Limited To?

You could describe the system as:

✅ A Pattern-Driven Proof Validator

“Finds invisible bugs in recursive Prolog-style code using only induction and pattern unfolding.”

Or more formally:

✅ A Structural Proof Checker for Recursive Logic Programs

Limitations (in description):

Limited to pure recursive definitions over inductively-defined data (lists, trees).
Assumes functions are total, deterministic, and do not involve side effects or performance metrics.

⸻

📌 Summary

Yes, you can model a simple, powerful system that:
	•	Uses just induction + pattern unfolding
	•	Detects invisible bugs (termination, correctness, tail recursion)
	•	Skips cost-analysis or side-effect modeling

Name suggestion:

Inductive Debugger – A structural proof-based system for catching recursive bugs in logic programs.

Tagline:

“If it doesn’t unfold, it doesn’t work.” ✅
```

* Run tests
- `swipl -s test_runner.pl`

* Test a file
```
./run_pid.sh <file.pl> [options]
```

* Options:
- --tail: Check for tail recursion
- --proof: Generate proof traces
- --verbose: Show detailed output including proof traces
