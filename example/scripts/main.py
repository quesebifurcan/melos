import argparse
import collections
import itertools
import json
import sys

sys.path.append('../py')

from collections import namedtuple

from abjad import *
from abjad.tools.scoretools import FixedDurationTuplet

from layout import (
    create_score_objects,
    apply_score_overrides,
    make_lilypond_file,
)

from melos import to_abjad

def set_tempi(score):
    curr_tempo = None
    for c in iterate(score).by_class((Container,)):
        tempo = to_abjad.get_named_annotation(c, 'tempo')
        if tempo and not tempo == curr_tempo:
            fst = next(topleveltools.iterate(c).by_class((Chord, Rest)))
            attach(Tempo((1,4), tempo), fst)
        if tempo:
            curr_tempo = tempo

def attach_ties(_, selection):
    try:
        attach(Tie(), selection)
    except:
        pass

def interpret_spanners(score):
    spanner_groups = collections.OrderedDict(
        (('groups', [attach_ties]),)
    )
    for name, fns in spanner_groups.items():
        for spanner in iterate(score).by_spanner(to_abjad.NotationSpanner):
            if spanner.key == name:
                fns = spanner_groups[spanner.key]
                for fn in fns:
                    fn(spanner.value, list(spanner.components))

def hide_superfluous_time_signatures(staves):
    curr = None
    for measure in iterate(next(iter(staves.values()))).by_class((Measure,)):
        if curr and curr.time_signature == measure.time_signature:
            override(measure).score.time_signature.stencil = False
        curr = measure

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', dest='input')
    parser.add_argument('--output', dest='output')

    args = parser.parse_args()

    with open(args.input, 'r') as infile:
        score = json.load(infile)

    template = create_score_objects()
    score = to_abjad.Score(score)
    sections = score.to_abjad()

    apply_score_overrides(template.score)

    for section in sections:
        for staff_container in section:
            score.apply_spanners(staff_container)
            interpret_spanners(staff_container)

    for i, section in enumerate(sections):
        for staff_container in section:
            container_name = to_abjad.get_named_annotation(staff_container, 'name')
            template.staves[container_name].append(staff_container)

    set_tempi(template.score)
    hide_superfluous_time_signatures(template.staves)

    lilypond_file = make_lilypond_file(
        template.score,
        title='Test',
        author='Anonymous',
    )
    persist(lilypond_file).as_ly(args.output)

if __name__ == '__main__':
    main()
