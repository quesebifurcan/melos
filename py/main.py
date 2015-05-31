import argparse
import json

from abjad import *
from abjad.tools.scoretools import FixedDurationTuplet

def make_lilypond_file(score):
    lilypond_file = lilypondfiletools.make_basic_lilypond_file(score)
    lilypond_file.default_paper_size = 'a3', 'landscape'
    lilypond_file.global_staff_size = 14
    lilypond_file.layout_block.indent = 0
    lilypond_file.layout_block.ragged_right = True
    lilypond_file.paper_block.ragged_bottom = True
    return lilypond_file

def is_tuplet(d):
    return not d.get('w-duration') == d.get('duration')

def make_note(d):
    num, denom = d.get('duration')
    events = d.get('events')
    pitches = set([x.get('pitch') for x in events])
    if "rest" in pitches:
        return Rest(Duration(num, denom))
    else:
        return Chord(pitches, Duration(num, denom))

def make_tuplet(d):
    num, denom = d.get('duration')
    return FixedDurationTuplet(Duration(num, denom), [])

def get_child_nodes(node):
    children = node.get('children')
    if children is not None:
        return children
    else:
        return []



def interpret_node(parent, node, is_measure=False):

    if node.get('events') is not None:
        parent.append(make_note(node))

    else:
        if is_measure:
            measure = Measure(tuple(node.get('duration')))
            parent.append(measure)
            parent = measure

        if is_tuplet(node):
            tuplet = make_tuplet(node)
            parent.append(tuplet)
            parent = tuplet

        for child in get_child_nodes(node):
            if child.get('events') is not None:
                note = make_note(child)
                parent.append(note)
            else:
                interpret_node(parent, child)

def main():

    parser = argparse.ArgumentParser()
    parser.add_argument('input_file')
    args = parser.parse_args()

    with open(args.input_file, 'r') as infile:
        sections = json.load(infile)

    staves = {
        'upper': Staff(),
        'lower': Staff(),
        'ped': Staff(),
        }

    parts = staves.keys()

    for section in sections:
        for part in section:
            part_name = part.get('part-name')
            events = part.get('events')
            top = staves.get(part_name)
            for node in events.get('children'):
                if part_name == 'upper':
                    is_measure = True
                else:
                    is_measure = False
                interpret_node(top, node, is_measure=is_measure)

    for staff in staves.values():
        attach(Tie(), staff[:])

    score = Score(
        [staves.get(part) for part in 'upper lower ped'.split()])
    lilypond_file = make_lilypond_file(score)
    f(lilypond_file)

    show(lilypond_file)
    persist(lilypond_file).as_midi('/Users/fred/Desktop/abcd.mid')


if __name__ == '__main__':
    main()
