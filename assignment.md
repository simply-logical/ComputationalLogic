## 2021 Assignment for COMSM0022 Computational Logic for Artificial Intelligence

The goal of the assignment is to extend the reasoning capabilities of the Prolexa Q&A assistant. Here are some reasoning patterns Prolexa cannot currently handle. 

Negation:

> Every teacher is happy. 
> Donald is not happy. 
> Therefore, Donald is not a teacher.

Disjunction: 

> Pixels are red, blue or green. 
> Pixie is a pixel. 
> Pixie is not blue. 
> Therefore, Pixie is red or green.

Existential quantification: 

> Some humans are geniuses.
> Geniuses win prizes. 
> Therefore, some humans win prizes.

Abduction: 

> Most people infected with COVID-19 experience loss of taste. 
> Peter experiences loss of taste. 
> Therefore, (it is likely that) Peter is infected with COVID-19. 

Default rules: 

> Most birds fly except penguins. 
> Tweety is a bird. 
> Therefore, assuming Tweety is not a penguin, Tweety flies. 

You can come up with your own examples, and also study combinations of these. 

The main starting point is `prolexa_engine.pl`, where you will find the `prove_rb` meta-interpreter. You will need to extend this meta-interpreter following the methods explained in the book. 

You can choose to work on your own or in pairs. By the assignment deadline you need to submit your code as a runnable Colab notebook and a short report explaining your approach. 
