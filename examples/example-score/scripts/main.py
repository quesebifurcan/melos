import argparse
import collections
import itertools
import json

from collections import namedtuple
from termcolor import colored

from abjad import *
from abjad.tools.scoretools import FixedDurationTuplet

from layout import (
    create_score_objects,
    apply_score_overrides,
    apply_accidentals,
    make_lilypond_file,
)

from melos import to_abjad
from melos import midi_output

def create_pulse(subdivisions, pattern, pitches, duration):
    count = int(duration / Duration((1, 4)))
    result = Container()
    for x in range(count):
        tuplet = FixedDurationTuplet((1, 4), [])
        pending_rest_duration = 0
        for y in range(subdivisions):
            mask = next(pattern)
            if mask == 0:
                pending_rest_duration += Duration((1, 16))
            if mask == 1:
                if pending_rest_duration:
                    tuplet.append(Rest(pending_rest_duration))
                tuplet.append(Chord(pitches, Duration((1, 16))))
                pending_rest_duration = 0
        if pending_rest_duration:
            tuplet.append(Rest(pending_rest_duration))
        result.append(tuplet)
    return result

def apply_pulse(group):
    if (isinstance(group[0], Chord)):
        notation = to_abjad.get_named_annotation(group[0], 'notation')
        # Make an iterator from 'pattern' which can be used for all
        # groups.
        pattern = itertools.cycle(notation['pattern'])
        for event in group:
            total_duration = event.written_duration
            pulse = create_pulse(
                notation['subdivisions'],
                pattern,
                event.written_pitches,
                total_duration,
            )
            detach(Tie, event)
            selection = select(event)
            mutate(selection).replace(pulse)

def apply_arpeggio(group):
    if len(group[0].written_pitches) > 1:
        sel = select(group[0])
        tuplet = FixedDurationTuplet((1, 4), [])
        coll = []
        for pitch in group[0].written_pitches:
            coll.append(pitch)
            tuplet.append(Chord(coll, Duration((1,8))))
        mutate(sel).replace(tuplet)

def set_tempi(score):
    curr_tempo = None
    for c in iterate(score).by_class((Container,)):
        tempo = to_abjad.get_named_annotation(c, 'tempo')
        if tempo and not tempo == curr_tempo:
            fst = next(topleveltools.iterate(c).by_class((Chord, Rest)))
            attach(Tempo((1,4), tempo), fst)
            curr_tempo = tempo

def add_staff_markup(staff):
    fst = next(topleveltools.iterate(staff).by_class((Chord, Rest)))
    text = to_abjad.get_named_annotation(staff, 'notation')
    attach(Markup(text, direction=Up), fst)
    attach(indicatortools.BarLine('||'), staff[-1])

def notation_grouper(x):
    try:
        ann = to_abjad.get_named_annotation(x, 'notation').get('type')
        return ann
    except:
        return None

def apply_notations(notation_data, score):
    fns = {
        'pulse': apply_pulse,
        'arpeggio': apply_arpeggio,
    }
    fn = fns[notation_data['type']]
    if not fn:
        raise Exception('No processing function implemented for notation "{}"'.format(k))
    fn(score)

def attach_ties(_, selection):
    attach(Tie(), selection)

def annotate_containers(score):
    fns = {
        'section_container': add_staff_markup
    }
    for c in iterate(score).by_class((Container, Voice)):
        score_id_ann = to_abjad.get_named_annotation(c, 'score_id')
        if score_id_ann:
            fn = fns.get(score_id_ann)
            if fn:
                fn(c)

def interpret_spanners(score):
    spanner_groups = collections.OrderedDict(
        (('groups', [attach_ties]),
         ('notation', [apply_notations]))
    )
    for name, fns in spanner_groups.items():
        for spanner in iterate(score).by_spanner(to_abjad.NotationSpanner):
            if spanner.key == name:
                fns = spanner_groups[spanner.key]
                for fn in fns:
                    fn(spanner.value, list(spanner.components))

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', dest='input')
    parser.add_argument('--output', dest='output')

    args = parser.parse_args()

    with open(args.input, 'r') as infile:
        score = json.load(infile)

    # TODO: score overrides
    template = create_score_objects()
    score = to_abjad.Score(score)
    sections = score.to_abjad()

    apply_score_overrides(template.score)

    for section in sections[:2]:
        for staff_container in section:
            score.apply_spanners(staff_container)
            interpret_spanners(staff_container)
            annotate_containers(staff_container)
            set_tempi(staff_container)

    for section in sections:
        for staff_container in section:
            container_name = to_abjad.get_named_annotation(staff_container, 'name')
            template.staves[container_name].append(staff_container)

    lilypond_file = make_lilypond_file(template.score, title='Test', author='Anonymous')
    show(lilypond_file)

if __name__ == '__main__':
    main()
