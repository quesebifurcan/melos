import argparse
import itertools
import json

from abjad import *
from abjad.tools.scoretools import FixedDurationTuplet

from termcolor import colored

# TODO: Add additional info to each segment (stylistic indications, tempi etc.)

def apply_score_overrides(score):
    set_(score).tuplet_full_length = True
    scheme = schemetools.Scheme('tuplet-number::calc-fraction-text')
    override(score).tuplet_number.text = scheme
    override(score).tuplet_bracket.direction = 'up'

    override(score).tie.minimum_length = 3

    override(score).spacing_spanner.uniform_stretching = True
    moment = schemetools.SchemeMoment(1, 8)
    set_(score).proportional_notation_duration = moment
    override(score).spacing_spanner.strict_note_spacing = True

    spacing_vector = layouttools.make_spacing_vector(0, 0, 30, 0)
    return score

def make_lilypond_file(score, title='', author=''):
    lilypond_file = lilypondfiletools.make_basic_lilypond_file(score)
    # GLOBAL
    lilypond_file.global_staff_size = 14
    lilypond_file.default_paper_size = 'a4', 'portrait'
    # HEADER BLOCK
    lilypond_file.header_block.title = Markup(title)
    lilypond_file.header_block.composer = Markup(author)
    lilypond_file.header_block.tagline = False
    # PAPER BLOCK
    lilypond_file.paper_block.ragged_bottom = True
    spacing_vector = layouttools.make_spacing_vector(0, 0, 8, 0)
    lilypond_file.paper_block.system_system_spacing = spacing_vector
    spacing_vector = layouttools.make_spacing_vector(0, 0, 20, 0)
    lilypond_file.paper_block.top_markup_spacing = spacing_vector
    # LAYOUT BLOCK
    lilypond_file.layout_block.left_margin = 10
    spacing_vector = layouttools.make_spacing_vector(0, 0, 8, 0)
    override(score).staff_grouper.staff_staff_spacing = spacing_vector
    spacing_vector = layouttools.make_spacing_vector(0, 0, 3, 0)
    override(score).vertical_axis_group.staff_staff_spacing = spacing_vector

    override(score).tuplet_bracket.padding = 1
    override(score).tuplet_bracket.staff_padding = 2
    set_(score).tuplet_full_length = True

    return lilypond_file

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('input_file')
    parser.add_argument('title')
    parser.add_argument('author')
    parser.add_argument('score_out')
    parser.add_argument('midi_out')
    args = parser.parse_args()

    print colored("Loading {}".format(args.input_file), 'cyan')
    with open(args.input_file, 'r') as infile:
        score_segments = json.load(infile)

    print colored("Creating staves and instruments...", 'cyan')
    upper_staff = Staff()

    attach(Clef('treble'), upper_staff)
    print colored("Parsing score...", 'cyan')
    from fractions import Fraction
    for i, phrase_group in enumerate(score_segments):
        print colored("    parsing segment {}...".format(i), 'green')

        for phrase in phrase_group:
            for notes in phrase:
                pitches = [x['pitch'] for x in notes]
                duration = Fraction(notes[0]['duration'])
                chord = Chord(pitches, duration)
                upper_staff.append(chord)
            upper_staff.append(Rest(Duration(1,4)))
        upper_staff.append(Rest(Duration(8,4)))

    # Attach ties.
    attach(Tie(), upper_staff[:])

    score = Score([upper_staff])
    print colored("Apply score overrides...", 'cyan')
    apply_score_overrides(score)

    lilypond_file = make_lilypond_file(
        score,
        args.title,
        args.author,
    )

    print colored("Persist score as pdf...", 'cyan')
    persist(lilypond_file).as_pdf(args.score_out)
    print colored("Persist score as midi...", 'cyan')
    persist(lilypond_file).as_midi(args.midi_out)


if __name__ == '__main__':
    main()
