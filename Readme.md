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





# Testing
Tests for added functionality are found in the [tests directory](./tests). Tests are also validated by on CircleCI's continuous integration server, see badge at the top of this readme.

[![CircleCI](https://circleci.com/gh/mattclifford1/ComputationalLogic/tree/prolexa-plus.svg?style=svg)](https://circleci.com/gh/mattclifford1/ComputationalLogic/tree/prolexa-plus)
