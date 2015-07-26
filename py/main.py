import argparse
import itertools
import json

from abjad import *
from abjad.tools.scoretools import FixedDurationTuplet

# TODO: Add additional info to each segment (stylistic indications, tempi etc.)

def is_tuplet(d):
    return not d.get('w-duration') == d.get('duration')

def is_leaf(node):
    return node.get('events') is not None

def apply_accidentals(staff):
    prev_chord = None
    prev_duplicates = []
    for event in iterate(staff).by_class((Chord, Rest)):
        if (isinstance(event, Rest) or len(event.written_pitches) < 2):
            prev_chord = None
            prev_duplicates = []
        # Don't force display of accidentals when the previous chord is repeated.
        elif (prev_chord and set(event.written_pitches) == set(prev_chord.written_pitches)):
            continue
        else:
            curr_dpcn = [x.diatonic_pitch_class_name for x in event.written_pitches]
            curr_dpcn_duplicates = []
            for k, g in itertools.groupby(sorted(curr_dpcn)):
                group = list(g)
                if len(group) > 1:
                    curr_dpcn_duplicates.append(k)
                for note_head in event.note_heads:
                    dpcn = note_head.written_pitch.diatonic_pitch_class_name
                    if dpcn in curr_dpcn_duplicates:
                        note_head.is_forced = True
                if prev_chord:
                    prev_dpcn = [x.diatonic_pitch_class_name for x in prev_chord.written_pitches]
                    for note_head in event.note_heads:
                        if (note_head.written_pitch.diatonic_pitch_class_name in prev_dpcn and
                            not note_head.written_pitch in prev_chord.written_pitches):
                            note_head.is_forced = True
                            continue
                        if note_head.written_pitch.diatonic_pitch_class_name in prev_duplicates:
                            note_head.is_forced = True
                prev_duplicates = curr_dpcn_duplicates
                prev_chord = event

def make_note(node):
    num, denom = node.get('duration')
    events = node.get('events')
    pitches = set([event.get('pitch') for event in events])
    if "rest" in pitches:
        return Rest(Duration(num, denom))
    else:
        return Chord(pitches, Duration(num, denom))

def make_tuplet(d):
    num, denom = d.get('duration')
    return FixedDurationTuplet(Duration(num, denom), [])

def interpret_node(parent, node, is_measure_root=False, tempo=None):
    if is_leaf(node):
        parent.append(make_note(node))
    else:
        if is_measure_root:
            measure = Measure(tuple(node.get('duration')))
            if tempo is not None:
                attach(Tempo((1, 4), tempo), measure)
            parent.append(measure)
            parent = measure

        if is_tuplet(node):
            tuplet = make_tuplet(node)
            parent.append(tuplet)
            parent = tuplet

        for child in node.get('children', []):
            if child.get('events') is not None:
                note = make_note(child)
                parent.append(note)
            else:
                interpret_node(parent, child)

def apply_score_overrides(score):
    set_(score).tuplet_full_length = True
    scheme = schemetools.Scheme('tuplet-number::calc-fraction-text')
    override(score).tuplet_number.text = scheme
    override(score).tuplet_bracket.direction = 'up'
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
    spacing_vector = layouttools.make_spacing_vector(0, 0, 1, 0)
    override(score).staff_grouper.staff_staff_spacing = spacing_vector
    spacing_vector = layouttools.make_spacing_vector(0, 0, 3, 0)
    override(score).vertical_axis_group.staff_staff_spacing = spacing_vector
    return lilypond_file

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('input_file')
    parser.add_argument('title')
    parser.add_argument('author')
    parser.add_argument('score_out')
    parser.add_argument('midi_out')
    args = parser.parse_args()

    with open(args.input_file, 'r') as infile:
        score_segments = json.load(infile)

    upper_staff = Staff()
    lower_staff = Staff()
    ped_staff = Staff()

    attach(Clef('treble'), upper_staff)
    attach(Clef('treble'), lower_staff)
    attach(Clef('bass'), ped_staff)

    manuals_instrument = instrumenttools.Harp(
        instrument_name=r'Manuals',
        short_instrument_name='Man.',
    )
    pedals_instrument = instrumenttools.Instrument(
        instrument_name=r'Pedals',
        short_instrument_name='Ped.',
    )
    manuals_group = scoretools.StaffGroup([upper_staff, lower_staff])
    manuals_group.context_name = 'PianoStaff'
    attach(manuals_instrument, manuals_group)
    attach(pedals_instrument, ped_staff)

    named_staff_dict = {
        'upper': upper_staff,
        'lower': lower_staff,
        'ped': ped_staff,
    }
    tempo = None
    for segment in score_segments:
        parts, curr_tempo = [segment.get(x) for x in ['parts', 'tempo']]
        if tempo == curr_tempo:
            curr_tempo = None
        else:
            tempo = curr_tempo
        for part in parts:
            part_name = part.get('part-name')
            events = part.get('events')
            top = named_staff_dict.get(part_name)
            is_measure_root = True
            for node in events.get('children'):
                interpret_node(
                    top,
                    node,
                    is_measure_root=is_measure_root,
                    tempo=curr_tempo,
                    )
                # Only set the time-signature of the upper-most staff.
                is_measure_root = False

    # Attach ties.
    for staff in (upper_staff, lower_staff, ped_staff):
        attach(Tie(), staff[:])
        apply_accidentals(staff)

    score = Score([manuals_group, ped_staff])
    apply_score_overrides(score)

    lilypond_file = make_lilypond_file(
        score,
        args.title,
        args.author,
    )

    # import sys; sys.exit()
    persist(lilypond_file).as_pdf(args.score_out)
    persist(lilypond_file).as_midi(args.midi_out)


if __name__ == '__main__':
    main()
