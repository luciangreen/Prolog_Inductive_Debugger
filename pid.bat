@echo off
REM Batch file wrapper for Prolog Inductive Debugger
REM Author: luciangreen
REM Date: 2025-06-21 21:13:08

REM Check if at least one argument (file) is provided
IF "%~1"=="" (
    REM No arguments provided, show usage
    swipl -s "%~dp0run_pid.pl"
    EXIT /B 1
)

REM Run the PID system with all provided arguments
swipl -q -s "%~dp0run_pid.pl" -- %*
EXIT /B %ERRORLEVEL%