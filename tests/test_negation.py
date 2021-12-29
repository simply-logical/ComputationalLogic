from pyswip import Prolog
import prolexa.meta_grammar as meta
import nltk
nltk.download('omw-1.4')

# init
pl = Prolog()
meta.reset_grammar()
meta.initialise_prolexa(pl)

def ask(input):
    return meta.standardised_query(pl, input)[0]['Output']

def test_negation():
    ask('matt is not happy')
    ans = ask('explain why matt is not a teacher')
    assert ans == 'matt is not happy ; every teacher is happy ; therefore matt is not a teacher'
