import argparse
import itertools
import json

from termcolor import colored

from abjad import *
from abjad.tools.scoretools import FixedDurationTuplet

# TODO: Get rid of these globals
REGISTRATION = None
REGISTRATIONS_DICT = {
    "A": "\\caps \\large REGISTRATION: \\large \\caps \\bold A",
    "B": "\\caps \\large REGISTRATION: \\large \\caps \\bold B",
}

#------------------------------------------------------------------------------
# LAYOUT
#------------------------------------------------------------------------------
def apply_score_overrides(score):
    set_(score).tuplet_full_length = True
    scheme = schemetools.Scheme('tuplet-number::calc-fraction-text')
    override(score).tuplet_number.text = scheme
    override(score).tuplet_bracket.direction = 'up'
    override(score).tie.minimum_length = 3
    override(score).spacing_spanner.uniform_stretching = True
    moment = schemetools.SchemeMoment(1, 12)
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
    lilypond_file.paper_block.left_margin = 12
    vertical_distance = 4
    spacing_vector = layouttools.make_spacing_vector(0, 0, 8, 0)
    lilypond_file.paper_block.system_system_spacing = spacing_vector
    spacing_vector = layouttools.make_spacing_vector(0, 0, 8, 0)
    lilypond_file.paper_block.top_markup_spacing = spacing_vector
    # LAYOUT BLOCK
    lilypond_file.layout_block.left_margin = 10
    spacing_vector = layouttools.make_spacing_vector(0, 0, vertical_distance, 0)
    override(score).staff_grouper.staff_staff_spacing = spacing_vector
    spacing_vector = layouttools.make_spacing_vector(0, 0, vertical_distance, 0)
    override(score).vertical_axis_group.staff_staff_spacing = spacing_vector
    override(score).tuplet_bracket.padding = 1
    override(score).tuplet_bracket.staff_padding = 2
    set_(score).tuplet_full_length = True
    return lilypond_file

def adjust_tie(chord, direction):
    override(chord).tie.details__horizontal_distance_penalty_factor = 100
    override(chord).tie.details__vertical_distance_penalty_factor = 100
    # override(chord).tie.minimum_length = 4
    # override(chord).tie.details__same_dir_as_stem_penalty = 30
    override(chord).tie.direction = direction

def adjust_ties(staff):
    leaves = list(iterate(staff).by_class((Chord, Rest)))
    for curr, next_ in zip(leaves[:], leaves[1:]):
        override(curr).tie.details__height_limit = 0.85
        override(curr).tie.minimum_length = 3
        if (isinstance(curr, Chord) and
            isinstance(next_, Chord) and
            len(next_.written_pitches) > 1):
            if has_only_upper_tied(curr, next_):
                adjust_tie(curr, 'up')
            elif has_only_lower_tied(curr, next_):
                adjust_tie(curr, 'down')

def has_only_upper_tied(curr, next_):
    curr_pitches, next_pitches = curr.written_pitches, next_.written_pitches
    common = set(curr_pitches).intersection(set(next_pitches))
    highest_next = max(x.numbered_pitch for x in next_pitches)
    return len(common) == 1 and list(common)[0].numbered_pitch == highest_next.numbered_pitch

def has_only_lower_tied(curr, next_):
    curr_pitches, next_pitches = curr.written_pitches, next_.written_pitches
    common = set(curr_pitches).intersection(set(next_pitches))
    highest_next = max(x.numbered_pitch for x in next_pitches)
    return len(common) == 1 and list(common)[0].numbered_pitch != highest_next.numbered_pitch

#------------------------------------------------------------------------------
# JSON -> ABJAD
#------------------------------------------------------------------------------
def is_tuplet(d):
    return not d.get('w-duration') == d.get('duration')

def is_leaf(node):
    return node.get('events') is not None

def make_note(node):
    global REGISTRATION
    num, denom = node.get('duration')
    events = node.get('events')
    pitches = set([event.get('pitch') for event in events])
    if "rest" in pitches:
        event = Rest(Duration(num, denom))
    else:
        event = Chord(pitches, Duration(num, denom))
    notation = events[0].get('notation')
    if notation and notation.get('registration') and not REGISTRATION == notation.get('registration'):
        registration = notation.get('registration')
        annotation = indicatortools.Annotation('registration', registration)
        REGISTRATION = registration
        registration_markup = REGISTRATIONS_DICT.get(registration)
        attach(Markup(str(registration_markup), direction='^'), event)
        attach(annotation, event)
    return event

def make_tuplet(d):
    num, denom = d.get('duration')
    return FixedDurationTuplet(Duration(num, denom), [])

def interpret_node(parent, node, is_measure_root=False, tempo=None):
    if is_leaf(node):
        note = make_note(node)
        parent.append(note)
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

def parse_segments(score_segments, staff_dict):
    tempo = None
    for i, segment in enumerate(score_segments):
        parts, curr_tempo = [segment.get(x) for x in ['parts', 'tempo']]
        if tempo == curr_tempo:
            curr_tempo = None
        else:
            tempo = curr_tempo
        for part in parts:
            part_name = part.get('part-name')
            events = part.get('events')
            top = staff_dict.get(part_name)
            is_measure_root = True
            notes_pre = list(iterate(top).by_class((Chord, Rest)))
            for node in events.get('children'):
                interpret_node(
                    top,
                    node,
                    is_measure_root=is_measure_root,
                    tempo=curr_tempo,
                )
                # Only set the time-signature of the upper-most staff.
                is_measure_root = False
            # Connect new elements with ties
            notes_post = list(iterate(top).by_class((Chord, Rest)))
            notes_new = []
            for note in notes_post:
                if not note in notes_pre:
                    notes_new.append(note)
            attach(Tie(), notes_new)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--title', default='Test')
    parser.add_argument('--author', default='Anonymous')
    parser.add_argument('--score-out', dest='score_out')
    parser.add_argument('--input-files', nargs='+', dest='input_files')
    args = parser.parse_args()

    # TODO: build instrument definitions dynamically.
    # Config files -- define overrides for specific instrument types
    upper_staff = Staff()
    lower_staff = Staff()
    ped_staff = Staff()

    upper_staff.is_simultaneous = True
    voice_1 = Voice()
    voice_2 = Voice()

    lower_staff.is_simultaneous = True
    voice_3 = Voice()
    voice_4 = Voice()
    voice_5 = Voice()

    override(voice_1).stem.direction = 'up'
    override(voice_2).stem.direction = 'down'
    override(voice_1).tie.direction = 'up'
    override(voice_2).tie.direction = 'down'
    override(voice_1).tuplet_bracket.direction = 'up'
    override(voice_2).tuplet_bracket.direction = 'down'

    override(voice_3).stem.direction = 'up'
    override(voice_4).stem.direction = 'down'
    override(voice_3).tie.direction = 'up'
    override(voice_4).tie.direction = 'down'
    override(voice_3).tuplet_bracket.direction = 'up'
    override(voice_4).tuplet_bracket.direction = 'down'

    upper_staff.append(voice_1)
    upper_staff.append(voice_2)
    lower_staff.append(voice_3)
    lower_staff.append(voice_4)
    ped_staff.append(voice_5)

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

    score_segments = []
    for input_file in args.input_files:
        with open(input_file, 'r') as infile:
            score_segments += json.load(infile)
    named_staff_dict = {
        'voice-1': voice_1,
        'voice-2': voice_2,
        'voice-3': voice_3,
        'voice-4': voice_4,
        'voice-5': voice_5
    }
    parse_segments(score_segments, named_staff_dict)

    # Attach ties.
    # TODO: conditionally adjust ties (multiple voices in one part?)
    for staff in (upper_staff, lower_staff, ped_staff):
        override(staff).time_signature.style = 'numeric'
        apply_accidentals(staff)
        adjust_ties(staff)

    score = Score([manuals_group, ped_staff])
    print colored("Apply score overrides...", 'cyan')
    apply_score_overrides(score)

    lilypond_file = make_lilypond_file(
        score,
        args.title,
        args.author,
    )

    print colored("Persist score as pdf...", 'cyan')
    persist(lilypond_file).as_pdf(args.score_out)

if __name__ == '__main__':
    main()
