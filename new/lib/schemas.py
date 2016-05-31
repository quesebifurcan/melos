from collections import namedtuple
from fractions import Fraction

note_params = (
    ('part', None),
    ('pitch', 0),
    ('count', 0),
    ('max_count', 99),
    ('group', ''),
    ('is_dissonance_contributor', True),
    ('is_merge_left', False),
    ('is_merge_right', False),
)
chord_params = (
    ('duration', Fraction(1, 1)),
    ('tempo', 60),
    ('is_phrase_end', False),
    ('events', []),
)
rhythm_tree_node_params = (
    ('duration', Fraction(1, 1)),
    ('chord', None),
    ('sum_of_leaves_duration', 0),
    ('children', []),
)

Note = namedtuple('Note', tuple(param[0] for param in note_params))
Note.__new__.__defaults__  = tuple(param[1] for param in note_params)

Chord = namedtuple('Chord', tuple(param[0] for param in chord_params))
Chord.__new__.__defaults__ = tuple(param[1] for param in chord_params)

RhythmTreeNode = namedtuple('RhythmTreeNode', tuple(param[0] for param in rhythm_tree_node_params))
RhythmTreeNode.__new__.__defaults__ = tuple(param[1] for param in rhythm_tree_node_params)
