# Interactive Examples
Our added functionality is displayed in an online google colab notebook. [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/mattclifford1/ComputationalLogic/blob/prolexa-plus/Prolexa_Plus_Demo_Notebook.ipynb)

# Extensions of prolexa

We worked on extending Prolexa to handle negation and existential quantification.

# Negation

Adding negation - queries of the type `is peter not happy` or responses `peter is not happy` - requires additions to `prolexa.pl`, which handles utterances, `prolexa_engine.pl` which conducts actual proofs and explanations, and `prolexa_grammar.pl`, which allows the use of natural language.

Firstly we extend `prove_question/3`, `prove_question/2`, and `explain_question/3`, to attempt the negative of a given query. For example, should the query be `is peter happy`, and the initial attempt to prove `happy(peter):-true` fails, prolexa now attempts to prove `not(happy(peter)) :- true`. In the case this succeeds it reponds, using additions to the grammar, `peter is not happy`. Should neither succeed it responds TODO --  `I'm not able to answer this` --.

`prove_question/2` is now as follows, with `prove_question/3`, and `explain_question/3` altered in the same manner:

```
prove_question(Query,Answer):-
	findall(R,prolexa:stored_rule(_SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; prove_rb(not(Query),Rulebase) ->
		transform(not(Query),Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = "Sorry, I don\'t think this is the case"
	).
```

Extending the grammar to handle queries was more complex. It needs to be able to deal not only with rules of the form `A(X):-B(X)`, `A(X) :- true` but also

- `not(A(X)):-not(B(X))`
- `not(A(X)):-B(X)`
- `A(X):-not(B(X))`
- `not(A(X)) :- true`

this required the addition of new determiners, verb phrases and questions for example:

```
determiner_neg(s,X=>H,X=>B,[(H:-not(B))]) --> [every].
determiner_neg(p,X=>H,X=>B,[(H:-not(B))]) --> [all].

question1(not(Q)) --> [is], proper_noun(N,X), [not], property(N,X=>Q).

neg_verb_phrase(s,M) --> [is, not],property(s,M).
neg_verb_phrase(p,M) --> [are, not],property(p,M).

```

We also add to the `prolexa.pl` main file to check if an added rule directly conflicts with an existing rule. For example `happy(peter)` should replace `not(happy(peter))` and vice versa. This is fairly simple to implement, and prevents conflicting rules in proofs:

```
% A. Utterance is a sentence
	( phrase(sentence(Rule),UtteranceList),
	  ( known_rule(Rule,SessionId) -> % A1. It follows from known rules
			atomic_list_concat(['I already knew that',Utterance],' ',Answer)
		; Rule = [(not(A) :- true)|_],
		 known_rule([(A:-true)],SessionId) -> % A2. It contradicts an existing rule
			retractall(prolexa:stored_rule(_,[(A:-true)])),
			atomic_list_concat(['I\'ll now remember that ',Utterance],' ',Answer),
			assertz(prolexa:stored_rule(SessionId,Rule))

		; Rule = [(A :- true)|_], % write_debug("trying negation, negative head"),
		  known_rule([(not(A):-true)],SessionId) -> % A2. It contradicts an existing rule
			retractall(prolexa:stored_rule(_,[(not(A):-true)])),
			atomic_list_concat(['I\'ll now remember that ',Utterance],' ',Answer),
			assertz(prolexa:stored_rule(SessionId,Rule))

	  ; otherwise -> % A3. It doesn't follow, so add to stored rules'
			assertz(prolexa:stored_rule(SessionId,Rule)),
			atomic_list_concat(['I will remember that',Utterance],' ',Answer)
	  )
```

After these additions, we use a rulebase such as

```
stored_rule(1,[(teacher(X):-happy(X))]).
stored_rule(1,[(not(teacher(X)):-not(happy(X)))]).
stored_rule(1,[(student(X):-not(teacher(X)))]).

stored_rule(1,[(teacher(peter):-true )]).
stored_rule(1,[(not(happy(pixie)):-true   )]).
```

to have interactions like

```
?- prolexa_cli.

prolexa> "is pixie a teacher".

*** query(teacher(pixie))

pixie is not a teacher

prolexa> "who is not a teacher".

*** query(not(teacher(_14358)))

pixie is not a teacher

prolexa> "explain why pixie is a student".

pixie is not happy ; if not teacher then not happy ; every stud
ent is not a teacher ; therefore pixie is a student

prolexa>
```

# Testing
Tests for added functionality are found in the [tests directory](./tests). Tests are also validated by on CircleCI's continuous integration server, see badge at the top of this readme.

[![CircleCI](https://circleci.com/gh/mattclifford1/ComputationalLogic/tree/prolexa-plus.svg?style=svg)](https://circleci.com/gh/mattclifford1/ComputationalLogic/tree/prolexa-plus)
