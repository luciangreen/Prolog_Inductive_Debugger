% Runner script for Prolog Inductive Debugger tests
:- initialization(main).

main :-
    writeln('Setting up Prolog Inductive Debugger test environment...'),
    
    % Load the setup script to create example files
    consult('setup_test_environment.pl'),
    
    writeln('Running tests...'),
    
    % Run the test cases
    consult('test_runner.pl'),
    
    % Give the user some time to read the output
    sleep(1),
    
    % Exit with success
    halt(0).