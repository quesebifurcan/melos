import argparse
import itertools
import json

from collections import namedtuple
from termcolor import colored

from abjad import *

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

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', dest='input')
    parser.add_argument('--output', dest='output')

    args = parser.parse_args()

    with open(args.input, 'r') as infile:
        score = json.load(infile, object_hook=dict_to_namedtuple)

    # TODO: score overrides
    # TODO: extend last note in section
    # TODO: phrasing slurs
    # apply_score_overrides(score_data.score)
    template = create_score_objects()
    lilypond_file = to_abjad.Score(score).to_abjad(template)

    def add_staff_markup(staff):
        fst = next(topleveltools.iterate(staff).by_class((Chord, Rest)))
        text = to_abjad.get_named_annotation(staff, 'notation')
        attach(Markup(text, direction=Up), fst)

    post_process = {
        'section_container': add_staff_markup
    }

    for x in iterate(lilypond_file).by_class((Container, Voice)):
        maybe_ann = to_abjad.get_named_annotation(x, 'score_id')
        if maybe_ann:
            proc = post_process.get(maybe_ann)
            if proc:
                proc(x)

    def grouper(x):
        try:
            return to_abjad.get_named_annotation(x, 'notations')[0].phrase_id
        except:
            return ''

    for k, v in itertools.groupby(iterate(lilypond_file).by_class((Chord, Rest)),
                                  grouper):
        group = list(v)
        if isinstance(group[0], Chord):
            pitches = set([x.written_pitches for x in group])
            if len(pitches) > 1:
                attach(spannertools.Slur(), group)

    topleveltools.override(lilypond_file).time_signature.style = 'numeric'
    show(lilypond_file)

if __name__ == '__main__':
    main()
