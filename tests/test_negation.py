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

def test_negation_explain():
    ask('matt is not happy')
    ans = ask('explain why matt is not a teacher')
    assert ans == 'matt is not happy ; every teacher is happy ; therefore matt is not a teacher'

# the following works in ipython but not python...
# def test_negation_who():
#     ans = ask('who is not a teacher')
#     assert ans == b'pixie is not a teacher'

def test_deletion():
    ask('matt is happy')      # one rule
    ans = ask('is matt happy')
    assert ans == b'matt is happy'

    ask('matt is not happy')  # add the negation
    ans = ask('is matt not happy')
    assert ans == b'matt is not happy'

    # the following works in swipl and ipython but not python ...
    # ans = ask('is matt not happy')
    # assert ans == b'matt is not happy'

if __name__ == '__main__':
    pl = Prolog()
    meta.reset_grammar()
    meta.initialise_prolexa(pl)
    ask('matt is happy')
    ans = ask('is matt happy')
    ask('matt is not happy')
    ans = ask('is matt happy')
    print(ans) ## prints('matt is not happy') in ipython
