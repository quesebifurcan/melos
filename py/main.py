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
from build_score import (
    parse_node,
    parse_segments,
)

def dict_to_namedtuple(d):
    key_replace = lambda k: k.replace('-', '_').replace('?', '_bool')
    return namedtuple('X', [key_replace(k) for k in d.keys()])(*d.values())

def make_score(args):
    score_segments = []
    with open(args.input_file, 'r') as infile:
        # TODO: use namedtuple
        score_segments = json.load(infile)
    staff = Staff()
    for x in score_segments:
        print x.get('tempo')
        measures = x.get('measures')
        for measure in measures:
            result = parse_node(measure)
            staff.append(result)
    attach_ties(staff)
    # TODO: schema for measure and score?
    print score_segments
    import sys; sys.exit()
    score_data = create_score_objects()
    # HERE BE SIDE EFFECTS
    segment_markup = {
        'tempo': {},
    }
    for s in score_segments:
        print s.get('section-name')
        # segment_markup['tempo'][i] = s.get('tempo')
        # print len(s['parts'][0]['events'])
        # print s.get('markup')
    parse_segments(score_segments, score_data.voices)
    for voice in score_data.voices:
        for measure in iterate(voice).by_class(Measure):
            print measure.name
        attach_ties(voice)
    print colored("Apply formatting...", 'cyan')
    apply_score_overrides(score_data.score)
    return score_data

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', dest='input')
    parser.add_argument('--output', dest='output')

    args = parser.parse_args()

    import sys
    if args.input:
        json_data = json.loads(args.input)
    else:
        for line in sys.stdin:
            json_data = json.loads(line)

    # for x in json_data[0].get('measures'):
    #     print x.keys()
    print json_data.get('title')
    print json_data.get('author')
    staff = Staff()
    for x in json_data.get('music'):
        for measure in x.get('measures'):
            result = parse_node(measure)
            staff.append(result)
    show(staff)
    import sys; sys.exit()

    print colored("Creating score...", 'cyan')
    score_data = make_score(args)
    print colored("Persist score as pdf...", 'cyan')

    # def get_annotation(elt, name):
    #     annotations = inspect_(elt).get_indicators(indicatortools.Annotation)
    #     annotations = filter(lambda x: x.name == name, annotations)
    #     if annotations:
    #         return annotations[0].value
    #     return None

    # def get_registration(elt):
    #     notation = get_annotation(elt, 'notation')
    #     if notation:
    #         return notation.get('registration', None)
    #     return None

    # for staff in score_data.staves:
    #     curr_registration = None
    #     for event in iterate(staff).by_class((Chord, Rest)):
    #         annotations = inspect_(event).get_indicators(indicatortools.Annotation)
    #         registration = get_registration(event)
    #         if not curr_registration == registration:
    #             markup = registration
    #             attach(Markup(markup, direction='^'), event)
    #         curr_registration = registration

    # TODO: tempo, registration, section headers, (spanners)
    lilypond_file = make_lilypond_file(
        score_data.score,
        args.title,
        args.author,
    )
    persist(lilypond_file).as_pdf(args.score_out)

if __name__ == '__main__':
    main()
