from collections import namedtuple
from fractions import Fraction
import pprint

durations = {
    1:              [Fraction(1, 2), Fraction(1, 2)],
    Fraction(1, 2): [Fraction(1, 4), Fraction(1, 4)]
}

def make_rtm_tree(template, node):
    nxt = durations.get(node)
    children = []
    if nxt:
        for child in nxt:
            children.append(make_rtm_tree(template, child))
    return {
        'duration': node,
        'chord': None,
        'children': children
    }

pp = pprint.PrettyPrinter(indent=2)

tree = make_rtm_tree(durations, 1)

notes = ['a', 'b', 'c', 'd', 'e', 'f']

def insert_chords(node):
    global notes
    if not notes:
        node['chord'] = 'rest'
        children = []
        for child in node.get('children'):
            children.append(insert_chords(child))
        node['children'] = children
        return node
    else:
        node['chord'] = notes[0]
        children = []
        for child in node.get('children'):
            notes = notes[1:]
            children.append(insert_chords(child))
        node['children'] = children
        return node

# print pp.pprint(
#     insert_chords(tree)
# )

# from abjad import *

# notes = [Note(0, Duration(1, 4)), Note(5, Duration(1, 4))]

def get_pitches(notes):
    coll = []
    for note in notes:
        coll.append(note.written_pitch.pitch_number)
    return coll

# print get_pitches(notes)

score = {
    'author': 'anon',
    'title': 'test',
    'sections': [
        {
            'template': 'asdf',
            'voices': {
                'a': 'tree'
            }
        }
    ]
}

print score
import itertools

a = {
    'a': [1, 2, 3],
    'b': [10, 20, 30, 40],
}

d2 = {k: itertools.cycle(v) for k, v in a.items()}

coll = []
for x in ['a', 'b', 'b', 'a', 'b', 'b', 'b', 'b', 'b', 'a']:
    coll.append(d2.get(x).next())

print coll

