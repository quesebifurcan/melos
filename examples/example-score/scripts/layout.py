from abjad import *
import itertools

from collections import namedtuple
ScoreData = namedtuple('ScoreData', ['score', 'staves', 'staff_to_voices_mapping'])

#------------------------------------------------------------------------------
# LAYOUT
#------------------------------------------------------------------------------
def apply_score_overrides(score):
    set_(score).tuplet_full_length = True
    override(score).time_signature.style = 'numeric'
    scheme = schemetools.Scheme('tuplet-number::calc-fraction-text')
    override(score).tuplet_number.text = scheme
    override(score).tuplet_bracket.direction = 'up'
    override(score).tie.minimum_length = 3

    # # Uniform stretching
    override(score).spacing_spanner.uniform_stretching = True
    moment = schemetools.SchemeMoment(1, 16)
    set_(score).proportional_notation_duration = moment
    override(score).spacing_spanner.strict_note_spacing = True

    return score

def make_lilypond_file(score, title='', author=''):
    lilypond_file = lilypondfiletools.make_basic_lilypond_file(score)
    # GLOBAL
    lilypond_file = new(
        lilypond_file,
        global_staff_size=14,
        default_paper_size=('a4', 'landscape'),
    )
    # HEADER BLOCK
    lilypond_file.header_block.title = Markup(title)
    lilypond_file.header_block.composer = Markup(author)
    lilypond_file.header_block.tagline = False
    # PAPER BLOCK
    lilypond_file.paper_block.ragged_bottom = True
    lilypond_file.paper_block.left_margin = 12
    vertical_distance = 1
    spacing_vector = schemetools.make_spacing_vector(0, 0, 6, 0)
    lilypond_file.paper_block.system_system_spacing = spacing_vector
    spacing_vector = schemetools.make_spacing_vector(0, 0, 8, 0)
    lilypond_file.paper_block.top_markup_spacing = spacing_vector
    # LAYOUT BLOCK
    lilypond_file.layout_block.left_margin = 10
    spacing_vector = schemetools.make_spacing_vector(0, 0, vertical_distance, 0)
    override(score).staff_grouper.staff_staff_spacing = spacing_vector
    spacing_vector = schemetools.make_spacing_vector(0, 0, vertical_distance, 0)
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

def have_common_pitches(a, b):
    if not a or not b:
        return False
    annotations = inspect_(a).get_indicators(indicatortools.Annotation)
    group_annotation_a = next(filter(lambda x: x.name == 'groups', annotations)).value
    annotations = inspect_(b).get_indicators(indicatortools.Annotation)
    group_annotation_b = next(filter(lambda x: x.name == 'groups', annotations)).value
    for k, v in group_annotation_a.items():
        if group_annotation_b.get(k) == v:
            return True
    return False

def groupby_common_note(events):
    coll = []
    curr_group = []
    for curr, next_ in itertools.zip_longest(events, events[1:]):
        # If we encounter a rest, continue after appending the pending
        # notes in "curr_group"
        if isinstance(curr, Rest):
            if curr_group:
                coll.append(curr_group)
            curr_group = []
            continue
        if len(curr_group) == 0:
            curr_group.append(curr)
        elif have_common_pitches(curr_group[-1], curr):
            curr_group.append(curr)
        else:
            coll.append(curr_group)
            curr_group = [curr]
    if curr_group:
        coll.append(curr_group)
    return coll

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

def attach_ties(voice):
    events = list(iterate(voice).by_class((Chord, Rest)))
    for group in groupby_common_note(events):
        attach(Tie(), group)
    override(voice).time_signature.style = 'numeric'
    apply_accidentals(voice)
    adjust_ties(voice)

def create_score_objects():
    staff_1 = Staff(name='1')
    staff_2 = Staff(name='2')
    staff_3 = Staff(name='3')
    staff_4 = Staff(name='4')
    staff_5 = Staff(name='5')

    # upper_staff.is_simultaneous = True
    # voice_1 = Voice(name='voice-1')
    # voice_2 = Voice(name='voice-2')

    # lower_staff.is_simultaneous = True
    # voice_3 = Voice(name='voice-3')
    # voice_4 = Voice(name='voice-4')
    # voice_5 = Voice(name='voice-5')

    # override(voice_1).stem.direction = 'up'
    # override(voice_2).stem.direction = 'down'
    # override(voice_1).tie.direction = 'up'
    # override(voice_2).tie.direction = 'down'
    # override(voice_1).tuplet_bracket.direction = 'up'
    # override(voice_2).tuplet_bracket.direction = 'down'

    # override(voice_3).stem.direction = 'up'
    # override(voice_4).stem.direction = 'down'
    # override(voice_3).tie.direction = 'up'
    # override(voice_4).tie.direction = 'down'
    # override(voice_3).tuplet_bracket.direction = 'up'
    # override(voice_4).tuplet_bracket.direction = 'down'

    # upper_staff.append(voice_1)
    # upper_staff.append(voice_2)
    # lower_staff.append(voice_3)
    # lower_staff.append(voice_4)
    # ped_staff.append(voice_5)

    attach(Clef('treble'), staff_1)
    attach(Clef('treble'), staff_2)
    attach(Clef('treble'), staff_3)
    attach(Clef('treble'), staff_4)
    attach(Clef('bass'), staff_5)

    # manuals_instrument = instrumenttools.Harp(
    #     instrument_name=r'Manuals',
    #     short_instrument_name='Man.',
    # )
    # pedals_instrument = instrumenttools.Instrument(
    #     instrument_name=r'Pedals',
    #     short_instrument_name='Ped.',
    # )
    # manuals_group = scoretools.StaffGroup([upper_staff, lower_staff])
    # manuals_group.context_name = 'PianoStaff'
    # attach(manuals_instrument, manuals_group)
    # attach(pedals_instrument, ped_staff)

    import collections

    staff_data = collections.OrderedDict((
        ('a', {'clef': 'treble'}),
        ('b', {'clef': 'treble'}),
        ('c', {'clef': 'treble'}),
        ('d', {'clef': 'treble'}),
        ('e', {'clef': 'bass'}),
    ))

    staves = collections.OrderedDict()
    for k, v in staff_data.items():
        staff = Staff(name=k)
        attach(Clef(v['clef']), staff)
        instrument = instrumenttools.Instrument(
            instrument_name=k.upper(),
            short_instrument_name=k.upper(),
        )
        attach(instrument, staff)
        staves[k] = staff

    group = scoretools.StaffGroup(staves.values())
    score = Score([group])
    return ScoreData(
        score=score,
        staves=staves,
        staff_to_voices_mapping={
            'voice-1': 'a',
            'voice-3': 'b',
            'voice-5': 'c'
        }
    )
