import contractions
import os
import re
import string
import requests


from enum import Enum
from prolexa import PACKAGE_PATH, PROLOG_PATH
from utils import Tagger, POS, CHUNK, standardise_tags,get_complex_tag,remove_punctuation, lemmatise, is_plural

PROLOG_DET_REGEX = r'determiner\([a-z],X=>B,X=>H,\[\(H:-B\)\]\)(.*)'
PROLOG_DET = 'determiner(p,X=>B,X=>H,[(H:-B)]) --> [{}].\n'


API_URI = 'http://api.conceptnet.io/'
EN = 'en'


class REL(Enum):
    IS_A = 'IsA'
    HAS_A = 'HasA'
    PART_OF = 'PartOF'
    USED_FOR = 'UsedFor'
    CAPABLE_OF = 'CapableOf'
    AT_LOCATION = 'AtLocation'
    CAUSES = 'Causes'
    HAS_PROPERTY = 'HasProperty'
    DESIRES = 'Desires'
    CREATED_BY = 'CreatedBy'
    CAUSES_DESIRE = 'CausesDesire'
    MADE_OF = 'MadeOf'

RELATIONS = [REL.IS_A.value, REL.HAS_A.value, REL.PART_OF.value,
        REL.USED_FOR.value,
        REL.CAPABLE_OF.value, REL.AT_LOCATION.value, REL.CAUSES.value,
        REL.HAS_PROPERTY.value, REL.DESIRES.value, REL.CREATED_BY.value,
        REL.CAUSES_DESIRE.value, REL.MADE_OF.value]


tagger = Tagger()

def get_prolog_grammar(path, fname):
    with open(os.path.join(path, fname), 'r') as f:
        lines = f.readlines()
    return lines

def get_prolog_rule(path, fname):
    with open(os.path.join(path, fname), 'r') as f:
        lines = f.readlines()
    return lines

def write_new_grammar(path, lines):
    with open(os.path.join(path, 'knowledge_store.pl'), 'w') as f:
        lines = ''.join(lines)
        f.write(lines)

def write_new_prolexa(path, lines):
    with open(os.path.join(path, 'prolexa.pl'), 'w') as f:
        lines = ''.join(lines)
        f.write(lines)

def initialise_prolexa(pl):
    #pl.consult(os.path.join(PROLOG_PATH, 'prolexa.pl'))
    pl.consult(os.path.join(PACKAGE_PATH, 'prolexa.pl'))


def standardised_query(pl, text):
    text = remove_punctuation(text)
    text = contractions.fix(text)
    text = lemmatise(text)
    message = fetch_common_sense_knowledge(pl, text)
    return message

def fetch_common_sense_knowledge(pl, input_):
    target, property_,description = get_target_property(input_)
    if target and property_:
        update_knowledge(tagger, target, property_, description)
        return 'I have also discovered more knowledge about {}'.format(target)
    
    return ''

def get_target_property(input_):
    grammar_1 = r'^(every|all)? *[\w]* *(is|are) (a|an)? *[a-z]* *$'
    if re.match(grammar_1, input_,re.IGNORECASE):
        target_regex=r"([\w]*) *(is|are)"
        property_regex=r"(is|are) ?(a|an)? ([\w]*) *"
        is_are = re.findall(target_regex, input_)[0][1]
        target = re.findall(target_regex, input_)[0][0]
        property_ = re.findall(property_regex, input_)[0][-1]
        description = target+' '+is_are+' '+property_
        return(target, property_, description)

    return (None,None,None)

def get_tags(tagger,text):
    _, _, tags = tagger.tag(text)
    tags = standardise_tags(tags)
    return tags

def get_property_pos(target, property_, description):
    tags = get_tags(tagger, description)
    words_tags = dict(zip(description.split(' '), tags))
    return words_tags
    
def update_knowledge(tagger, target, property_, description):
    # CHECK IF THE PROPERTY IS NOUN
    words_tags = get_property_pos(target, property_, description)
    if words_tags[property_] == POS.NOUN.value:
        print('Acquiring common sense knowledge ...')

        # get common_sense and update rules
        common_sense_knowledge = fetch_word_info(property_, RELATIONS)
        
        lines_knowledge_store = get_prolog_grammar(PACKAGE_PATH, 'knowledge_store.pl')
        
        lines_prolexa_rules = get_prolog_rule(PACKAGE_PATH, 'prolexa.pl')

        add_rules_knowledge(target, property_,
                common_sense_knowledge,lines_knowledge_store,
                lines_prolexa_rules)
        print('Rules were added successfully!')
      
def add_rules_knowledge(target, property_, common_sense_knowledge,
        lines_knowledge_store, lines_prolexa_rules):
    for rel, data in common_sense_knowledge.items():
        if rel == REL.IS_A.value:
            for label in data:
                complex_tag, content = get_complex_tag(label)
                knowledge_store_updated, prolexa_rules_updated = generate_rule_knowledge(complex_tag, content, property_,
                        lines_knowledge_store, lines_prolexa_rules)
                if knowledge_store_updated:
                    write_new_grammar(PACKAGE_PATH, knowledge_store_updated)
                if prolexa_rules_updated:
                    write_new_prolexa(PACKAGE_PATH, prolexa_rules_updated)

        elif rel == REL.HAS_A.value:
            pass
            # check for the complexity of the grammar
            # add rule accordingly


def generate_rule_knowledge(complex_tag, content, property_,
        lines_knowledge_store, lines_prolexa_rules):
    if complex_tag == CHUNK.NN.value:
        knowledge_updated = handle_noun_knowledge(lines_knowledge_store,content, complex_tag)
        rule_updated = handle_noun_rule(property_, content, lines_prolexa_rules)
        return knowledge_updated, rule_updated
    
    elif complex_tag == CHUNK.JJ.value:
        knowledge_updated = handle_adj_knowledge(lines_knowledge_store,content, complex_tag)
        rule_updated = handle_adj_rule(property_, content, lines_prolexa_rules)
        return knowledge_updated, rule_updated
    
    elif complex_tag == CHUNK.VB.value:
        pass
        #handle_verb(input_word, label, lines)

    elif complex_tag == CHUNK.CNP.value:
        pass
        #handle_complex_noun_phrase(input_word, label, lines)

    elif complex_tag == CHUNK.CVP.value:
        pass
        #handle_complex_verb_phrase(input_word, label, lines)

    return None, None

def handle_noun_rule(target, property_, lines): 
    rule = 'stored_rule(1,[('+property_+'(X):-'+target+'(X))]).'
    rule_reg = r'stored_rule\(1,\[\({}\(X\):-{}\(X\)\)\]\).'.format(property_,
            target)
    i  = len(lines)
    rule_match = r'stored_rule\(1,\[\((.*)\(X\):-(.*)\(X\)\)\]\).'
    for idx, line in enumerate(iter(lines)):
        if re.match(rule_reg, line):
            return lines
        
        if re.match(rule_match, line):
            i = idx

    lines.insert(i+1, rule+'\n')
    return lines

def handle_adj_rule(target, property_, lines): 
    rule = 'stored_rule(1,[('+property_+'(X):-'+target+'(X))]).'
    rule_reg = r'stored_rule\(1,\[\({}\(X\):-{}\(X\)\)\]\).'.format(property_,
            target)
    i  = len(lines)
    rule_match = r'stored_rule\(1,\[\((.*)\(X\):-(.*)\(X\)\)\]\).'
    for idx, line in enumerate(iter(lines)):
        if re.match(rule_reg, line):
            return lines
        
        if re.match(rule_match, line):
            i = idx
    lines.insert(i+1, rule+'\n')
    return lines

def handle_noun_knowledge(lines, property_, complex_tag):
    text = [property_]
    tags = [complex_tag]
    
    i = len(lines)
    for idx, line in enumerate(iter(lines)):
        pred_match = r'pred\((.*)[1],\[(.*)\]\)\.'
        if re.match(pred_match, line):
            i = idx
            break
    lines_updated = handle_noun(lines, i, text, tags)
    return lines_updated


def handle_adj_knowledge(lines, property_, complex_tag):
    text = [property_]
    tags = [complex_tag]
    
    i = len(lines)
    for idx, line in enumerate(iter(lines)):
        pred_match = r'pred\((.*)[1],\[(.*)\]\)\.'
        if re.match(pred_match, line):
            i = idx
    lines_updated = handle_adjective(lines, i, text, tags)
    return lines_updated


def handle_noun(lines, i, text, tags):
    nn = POS.NOUN.value
    start = 'pred('
    end = ', '
    exists = False
    new_line = ''
    input_word = text[tags.index(nn)]
    _, input_word = is_plural(input_word)
    text[tags.index(nn)] = input_word

    for noun_idx, noun_line in enumerate(lines[i:]):
        if not(re.match(r'pred\((.*)[1],\[(.*)\]\)\.', noun_line)):
            noun_idx = noun_idx + i
            if tags:
                tags.remove(nn)
            if text:
                text.remove(input_word)
            break

        line_word = (noun_line.split(start))[1].split(end)[0]
        if input_word == line_word:
            if (re.match(r'pred\((.*)[1](.*)n\/(.*)\]\)\.', noun_line)):
                exists = True
                if tags:
                    tags.remove(nn)
                if text:
                    text.remove(input_word)
                break
            else:
                noun_idx = noun_idx + i
                insert_idx = noun_line.index(']).')
                new_line = (noun_line[:insert_idx]
                            + ',n/'
                            + input_word
                            + noun_line[insert_idx:])
                lines[noun_idx] = new_line
                exists = True
                if tags:
                    tags.remove(nn)
                if text:
                    text.remove(input_word)
                break

    if not exists:
        if new_line == '':
            new_line = 'pred(' + input_word + ', 1,[n/' + input_word + ']).\n'
        lines.insert(noun_idx, new_line)
        

    return lines


def handle_adjective(lines, i, text, tags):
    a = POS.ADJECTIVE.value
    start = 'pred('
    end = ', '
    exists = False
    new_line = ''
    input_word = text[tags.index(a)]
    _, input_word = is_plural(input_word)
    text[tags.index(a)] = input_word

    for noun_idx, noun_line in enumerate(lines[i:]):
        if not(re.match(r'pred\((.*)[1],\[(.*)\]\)\.', noun_line)):
            noun_idx = noun_idx + i
            if tags:
                tags.remove('JJ')
            if text:
                text.remove(input_word)
            break
        line_word = (noun_line.split(start))[1].split(end)[0]
        if input_word == line_word:
            if (re.match(r'pred\((.*)[1](.*)a\/(.*)\]\)\.', noun_line)):
                exists = True
                if tags:
                    tags.remove(a)
                if text:
                    text.remove(input_word)
                break
            else:
                noun_idx = noun_idx + i
                insert_idx = noun_line.index(']).')
                new_line = (noun_line[:insert_idx]
                            + ',a/'
                            + input_word
                            + noun_line[insert_idx:])
                lines[noun_idx] = new_line
                exists = True
                if tags:
                    tags.remove(a)
                if text:
                    text.remove(input_word)
                break

    if not exists:
        if new_line == '':
            new_line = 'pred(' + input_word + ', 1,[a/' + input_word + ']).\n'
        lines.insert(noun_idx, new_line)

    return lines



def fetch_word_info(property_, relations):
    data={}
    for rel in relations:
        query = 'query?start=/c/{}/{}&rel=/r/{}&limit=1000'.format(EN,property_,rel)
        obj = requests.get(API_URI+query).json()
        
        # check if such a relation exist
        if len(obj['edges']) == 0:
            continue
        info = extract_info(obj)
        data[rel] = info
    return data

def extract_info(obj):
    labels = []
    for edge in obj['edges']:
        labels.append(edge['end']['label'])
        
    return labels
