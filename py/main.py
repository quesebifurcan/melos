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
    type_ = d.get('type', 'NoType')
    key_replace = lambda k: k.replace('-', '_').replace('?', '_bool')
    return namedtuple(type_, [key_replace(k) for k in d.keys()])(*d.values())

def make_score(args):
    score_segments = []
    with open(args.input, 'r') as infile:
        score_segments.append(json.load(infile))

    score_data = create_score_objects()
    # HERE BE SIDE EFFECTS
    segment_markup = {
        'tempo': {},
    }

    # TODO
    parse_segments(score_segments[0]['sections'], score_data.voices)
    print(score_data.voices)

    for voice in score_data.voices:
        for measure in iterate(voice).by_class(Measure):
            print(measure.name)
        attach_ties(voice)
    print(colored("Apply formatting...", 'cyan'))
    apply_score_overrides(score_data.score)
    return score_data

# class Voice_:
#     blueprint = {}
#     def __init__(self, data):
#         self.asdf = init(self, data)
#     @staticmethod
#     def get_data(x):
#         return [1,2,3,4,5,6]
#     def from_namedtuple(self):
#         return self.asdf

# def get_converter(name):
#     return {
#         'Voice'  : Voice_,
#         'Section': Section,
#         'Score'  : Score,
#     }.get(name)

# def init(self, data):
#     result = type('temp', (object,), {})()
#     for k, v in self.blueprint.items():
#         if isinstance(v, list):
#             lst = []
#             converter = get_converter(v[0])
#             for x in converter.get_data(data):
#                 lst.append(converter(x).from_namedtuple())
#             setattr(result, k, lst)
#     return result

# # TODO: all shapes must be accessible via either:
# # 1. x.y or
# # 2. map(fn, x.y)
# class Section:
#     blueprint = {
#         'voices': ['Voice']
#     }
#     @staticmethod
#     def get_data(x):
#         return getattr(x, 'sections')
#     def __init__(self, data):
#         self.asdf = init(self, data)
#     def from_namedtuple(self):
#         return self.asdf.voices

def get_converter(k):
    return {
        'sections': Sections,
        'staves': Staves,
        'voices': Voices,
        'measures': Measures,
        'children': Children,
        'duration': lambda x: Fraction(x),
    }.get(k)

import to_abjad

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', dest='input')
    parser.add_argument('--output', dest='output')

    args = parser.parse_args()

    with open(args.input, 'r') as infile:
        score = json.load(infile, object_hook=dict_to_namedtuple)

    # print(Score(score).to_abjad())
    abjad_score = to_abjad.convert(score)
    print(abjad_score)
    print(len(score.sections))
    import sys; sys.exit()
    print(data)

    # print(colored("Creating score...", 'cyan'))
    # score_data = make_score(args)
    # print(colored("Persist score as pdf...", 'cyan'))

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

    # # TODO: tempo, registration, section headers, (spanners)
    # lilypond_file = make_lilypond_file(
    #     score_data.score,
    #     'a', 'b',
    #     # data.title,
    #     # data.author,
    # )
    # show(score_data.score)
    # # persist(lilypond_file).as_pdf(args.output)

if __name__ == '__main__':
    main()
