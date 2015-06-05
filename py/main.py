import argparse
import json

from abjad import *
from abjad.tools.scoretools import FixedDurationTuplet

def make_lilypond_file(score):
    lilypond_file = lilypondfiletools.make_basic_lilypond_file(score)
    lilypond_file.default_paper_size = 'a4', 'portrait'
    lilypond_file.global_staff_size = 14
    # lilypond_file.layout_block.indent = 10
    # lilypond_file.layout_block.ragged_right = True
    lilypond_file.paper_block.ragged_bottom = True
    lilypond_file.header_block.composer = Markup('Fredrik Wallberg')
    lilypond_file.header_block.title = Markup('Map')
    # lilypond_file.paper_block.top_margin = 20
    # lilypond_file.paper_block.top_margin_spacing = 100
    lilypond_file.layout_block.left_margin = 10
    spacing_vector = layouttools.make_spacing_vector(0, 0, 8, 0)
    lilypond_file.paper_block.system_system_spacing = spacing_vector
    spacing_vector = layouttools.make_spacing_vector(0, 0, 20, 0)
    lilypond_file.paper_block.top_markup_spacing = spacing_vector
    lilypond_file.header_block.tagline = False
    spacing_vector = layouttools.make_spacing_vector(0, 0, 1, 0)
    override(score).staff_grouper.staff_staff_spacing = spacing_vector
    spacing_vector = layouttools.make_spacing_vector(0, 0, 3, 0)
    override(score).vertical_axis_group.staff_staff_spacing = spacing_vector
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

def calculate_clef(pitches):
    avg = sum(pitches) / len(pitches)
    if avg < -5:
        return 'bass'
    else:
        return 'treble'

from itertools import izip_longest

def grouper(iterable, n, fillvalue=None):
    args = [iter(iterable)] * n
    return izip_longest(*args, fillvalue=fillvalue)

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

    attach(Clef('bass'), staves.get('ped'))
    upper, lower, ped = [staves.get(x) for x in ['upper', 'lower', 'ped']]
    group = scoretools.StaffGroup([upper, lower])
    man_instr = instrumenttools.Harp(instrument_name=r'Manuals',
                                       short_instrument_name='Man.')
    ped_instr = instrumenttools.Instrument(instrument_name=r'Pedals',
                                       short_instrument_name='Ped.')
    group.context_name = 'PianoStaff'
    # organ_instr = instrumenttools.Harp()
    attach(man_instr, group)
    attach(ped_instr, ped)

    f(group)
    # import sys; sys.exit()
    # staves.get('upper').consists_commands.append('Horizontal_bracket_engraver')

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
        override(staff).time_signature.style = 'numeric'

        current_clef = 'treble'
        for chords in grouper(iterate(staff).by_class((Chord, )), 5):

            pitches = []
            for chord in chords:
                if chord is not None:
                    pitches.extend([x.numbered_pitch.pitch_number for x in chord.written_pitches])
            clef = calculate_clef(pitches)
            if not current_clef == clef:
                attach(Clef(clef), chords[0])
                current_clef = clef

    score = Score(
        [group, staves.get('ped')])
    set_(score).tuplet_full_length = True
    # override(score).tuplet_bracket.padding = 2
    # override(score).tuplet_bracket.staff_padding = 4
    scheme = schemetools.Scheme('tuplet-number::calc-fraction-text')
    override(score).tuplet_number.text = scheme
    override(score).tuplet_bracket.direction = 'up'
    spacing_vector = layouttools.make_spacing_vector(0, 0, 30, 0)
    # override(score).vertical_axis_group.staff_staff_spacing = spacing_vector
    # override(score).staff_grouper.staff_staff_spacing = spacing_vector

    attach(Tempo((1, 4), 132), upper)

    lilypond_file = make_lilypond_file(score)

    show(lilypond_file)
    # persist(lilypond_file).as_midi('/Users/fred/Desktop/abcd.mid')


if __name__ == '__main__':
    main()
