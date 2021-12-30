prove_rb(not(B),Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
	prove_rb(not(A),Rulebase,[p(not(B),Rule)|P0],P).

A(X):-B(X).
not(A(X)):-not(B(X)).

A(X):-not(B(X)).
therefore
B(X):-not(A(X)).
