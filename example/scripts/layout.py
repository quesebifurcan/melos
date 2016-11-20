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
    spacing_vector = schemetools.make_spacing_vector(0, 0, 10, 0)
    lilypond_file.paper_block.system_system_spacing = spacing_vector
    spacing_vector = schemetools.make_spacing_vector(0, 0, 8, 0)
    lilypond_file.paper_block.top_markup_spacing = spacing_vector
    # LAYOUT BLOCK

    # Remove empty staves
    context_block = lilypondfiletools.ContextBlock(
        source_context_name=r'Staff \RemoveEmptyStaves',
    )
    override(context_block).vertical_axis_group.remove_first = True
    lilypond_file.layout_block.items.append(context_block)

    lilypond_file.layout_block.left_margin = 10
    spacing_vector = schemetools.make_spacing_vector(0, 0, vertical_distance, 0)
    override(score).staff_grouper.staff_staff_spacing = spacing_vector
    spacing_vector = schemetools.make_spacing_vector(0, 0, vertical_distance, 0)
    override(score).vertical_axis_group.staff_staff_spacing = spacing_vector
    override(score).tuplet_bracket.padding = 1
    override(score).tuplet_bracket.staff_padding = 2
    set_(score).tuplet_full_length = True
    return lilypond_file

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

    attach(Clef('treble'), staff_1)
    attach(Clef('treble'), staff_2)
    attach(Clef('treble'), staff_3)
    attach(Clef('treble'), staff_4)
    attach(Clef('bass'), staff_5)

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
