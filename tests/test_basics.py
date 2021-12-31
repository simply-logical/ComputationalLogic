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

def test_rule():
    ans = ask('is peter a teacher')
    assert ans == b'peter is a teacher'

def test_who():
    ans = ask('who is a teacher')
    assert ans == b'peter is a teacher'
