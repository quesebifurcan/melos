import argparse
import itertools
import json

from collections import namedtuple
from termcolor import colored

from abjad import *
from abjad.tools.scoretools import FixedDurationTuplet

from layout import (
    create_score_objects,
    attach_ties,
    apply_score_overrides,
    make_lilypond_file,
)

from melos import to_abjad

def dict_to_namedtuple(d):
    type_ = d.get('type', 'NoType')
    key_replace = lambda k: k.replace('-', '_').replace('?', '_bool')
    return namedtuple(type_, [key_replace(k) for k in d.keys()])(*d.values())

def apply_arpeggio(score):
    def grouper(x):
        try:
            ann = set(to_abjad.get_named_annotation(x, 'groups'))
            return ann
        except:
            return ''
    for k, v in itertools.groupby(iterate(score).by_class((Chord, Rest)), grouper):
        group = list(v)
        if (isinstance(group[0], Chord)):
            notations = filter(lambda x: x,
                               to_abjad.get_named_annotation(group[0], 'notations'))
            notations = set([x.type for x in notations])
            if 'arpeggio' in notations:
                if len(group[0].written_pitches) > 1:
                    sel = select(group[0])
                    tuplet = FixedDurationTuplet((1, 4), [])
                    coll = []
                    for pitch in group[0].written_pitches:
                        coll.append(pitch)
                        tuplet.append(Chord(coll, Duration((1,8))))
                    mutate(sel).replace(tuplet)

def create_pulse(pitches, duration):
    count = int(duration / Duration((1, 4)))
    result = []
    for x in range(count):
        tuplet = FixedDurationTuplet((1, 4), [])
        for y in range(4):
            tuplet.append(Chord(pitches, Duration((1, 16))))
        result.append(tuplet)
    return result

def apply_pulse(score):
    def grouper(x):
        try:
            ann = set(to_abjad.get_named_annotation(x, 'groups'))
            return ann
        except:
            return ''
    for k, v in itertools.groupby(iterate(score).by_class((Chord, Rest)), grouper):
        group = list(v)
        if (isinstance(group[0], Chord)):
            notations = filter(lambda x: x,
                               to_abjad.get_named_annotation(group[0], 'notations'))
            notations = (x for x in notations)
            for notation in notations:
                if notation.__class__.__name__ == 'pulse':
                    coll = []
                    for event in group:
                        total_duration = event.written_duration
                        pulse = Container(
                            create_pulse(event.written_pitches, total_duration)
                        )
                        coll.append(pulse)
                        selection = select(event)
                        mutate(select(event)).replace(pulse)
                for chord, mask in zip(iterate(coll).by_class((Chord, )),
                                       itertools.cycle(notation.pattern)):
                    if mask == 0:
                        mutate(chord).replace(Rest((1, 16)))

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', dest='input')
    parser.add_argument('--output', dest='output')

    args = parser.parse_args()

    with open(args.input, 'r') as infile:
        score = json.load(infile, object_hook=dict_to_namedtuple)

    # TODO: score overrides
    template = create_score_objects()
    score = to_abjad.Score(score).to_abjad(template)
    apply_score_overrides(score)

    def add_staff_markup(staff):
        fst = next(topleveltools.iterate(staff).by_class((Chord, Rest)))
        text = to_abjad.get_named_annotation(staff, 'notation')
        attach(Markup(text, direction=Up), fst)
        attach(indicatortools.BarLine('||'), staff[-1])

    post_process = {
        'section_container': add_staff_markup
    }

    for x in iterate(score).by_class((Container, Voice)):
        maybe_ann = to_abjad.get_named_annotation(x, 'score_id')
        if maybe_ann:
            proc = post_process.get(maybe_ann)
            if proc:
                proc(x)

    # apply_arpeggio(score)
    apply_pulse(score)

    lilypond_file = make_lilypond_file(score)
    show(lilypond_file)

if __name__ == '__main__':
    main()
