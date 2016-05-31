from collections import namedtuple
from fractions import Fraction
import itertools
import pprint

import lib
from new.lib import chord, chord_seq, utils, measure, schemas

from score import layout

import uuid

top = uuid.uuid4()

def new_voice(part_name, transposition, length):
    coll = []
    for i in range(length):
        group=uuid.uuid4()
        chord = schemas.Chord(
            duration=Fraction(2, 4),
            tempo=132,
            events=[
                schemas.Note(pitch=(i+transposition),
                     group=group,
                     part=part_name),
                # schemas.Note(pitch=7,
                #      group=top,
                #      part=part_name),
                ]
        )
        coll.append(chord)
    return coll

voice_1 = new_voice('voice_1', -10, 10)
voice_2 = new_voice('voice_2', -5, 7)
voice_3 = new_voice('voice_3', 0, 4)

voices = {
    'voice_1': voice_1,
    'voice_2': voice_2,
    'voice_3': voice_3,
}

from abjad.tools.scoretools import (Rest, Voice, FixedDurationTuplet, Container)
from abjad.tools import indicatortools
from abjad.tools import topleveltools
from abjad.tools import spannertools
from abjad.tools.scoretools import Chord as AbjadChord

def is_tied(a, b):
    a_ = topleveltools.inspect_(a).get_indicators(indicatortools.Annotation)[0].value
    b_ = topleveltools.inspect_(b).get_indicators(indicatortools.Annotation)[0].value
    return any([x in b_ for x in a_])

def get_tie_groups(xs):
    curr = []
    result = []
    for x in xs:
        if not curr:
            curr.append(x)
        elif is_tied(x, curr[-1]):
            curr.append(x)
        else:
            result.append(curr)
            curr = [x]
    if curr:
        result.append(curr)
    return result

def to_abjad(parent, node):
    if node.chord and node.chord == 'rest':
        parent.append(Rest(node.duration))
    elif node.chord:
        pitches = [note.pitch for note in node.chord.events]
        groups = sorted([note.group for note in node.chord.events])
        chord = AbjadChord(pitches, node.duration)
        groups_annotation = indicatortools.Annotation('groups', groups)
        # TODO: separate library code from the piece itself -- directory structure
        topleveltools.attach(groups_annotation, chord)
        parent.append(chord)
    else:
        tupl = FixedDurationTuplet(node.duration, [])
        result = []
        for child in node.children:
            result.append(to_abjad(tupl, child))
        parent.append(tupl)
    return parent

event_seqs = chord_seq.cycle_event_seqs(
    [
        'voice_1',
        'voice_2',
        'voice_3',
        'voice_1',
        'voice_2',
        'voice_3',
    ],
    voices)

def main():
    pp = pprint.PrettyPrinter(indent=2, width=80)
    # print 'test'
    # print schemas.Note(pitch=4)
    # print schemas.Chord(events=[schemas.Note(pitch=4)])
    # print pp.pprint(event_seqs)
    # print chord_seq.forward_time(
    #     voice_1[0],
    # )
    # print utils.update(voice_1[0], 'duration', lambda x: 123)
    # print utils.assoc(voice_1[0], 'duration', 987123).duration
    # print chord.remove_parts('voice_1', voice_1[0])

    # print chord_seq.merge_chords(
    #     voice_1[0],
    #     voice_2[0],
    # )

    default_mapping = {0: 0,
                       1: 10,
                       2: 4,
                       3: 3,
                       4: 2,
                       5: 1,
                       6: 5}

    # print(chord.pitch_to_pitchclass(-11))
    # print(chord.inversion_equivalent_pitchclass(8))
    # print(chord.all_intervals([1, 2, 3]))
    # print(chord.dissonance_value(default_mapping, [1, 2]))
    # print(chord.scaled_dissonance_value(default_mapping, [1, 2]))
    # print(chord.scaled_dissonance_value(default_mapping, [0, 1, 2]))
    # print(chord.is_consonant(default_mapping, [0, 2, 4], [0, 2]))
    # print(chord.is_consonant(default_mapping, [0, 2, 4], [0, 2, 4, 5]))

    def handle_dissonance(limit):
        def inner(a, b):
            if b.is_phrase_end:
                pitches = chord.select_chord_key(
                    'pitch',
                    chord_seq.merge_chords(a, b)
                )
                if chord.is_consonant(default_mapping, limit, pitches):
                    return chord_seq.merge_chords(a, b)
                else:
                    return b
            return chord_seq.merge_chords(a, b)
        return inner

    # TODO: multiple resolutions of the same duration?
    durations = {
        Fraction(1, 1): [Fraction(3, 4), Fraction(1, 2)],
        Fraction(3, 4): [(Fraction(1, 4), 'b'), Fraction(1, 4), Fraction(1, 4)],
        Fraction(1, 2): [Fraction(1, 4), Fraction(1, 4), Fraction(1, 4)],
        (Fraction(1, 4), 'b'): [Fraction(1, 8), Fraction(1, 8)],
        Fraction(1, 4): [Fraction(1, 8), Fraction(1, 8), Fraction(1, 8)],
        Fraction(1, 8): [Fraction(1, 16), Fraction(1, 16)],
    }

    # print(list(itertools.accumulate(
    #     [voice_1[0], voice_2[0],
    #      voice_1[1],
    #      voice_1[2],
    #      voice_2[1]],
    #     handle_dissonance([0, 2, 4]),
    # )))

    # print(measure.make_rtm_tree(durations, 1))
    # print(measure.get_summed_durations(
    #     durations,
    #     1,
    # ))

    # print('_____________')

    # tree.insert_chords(tree.tree)

    # tree = measure.Tree(
    #     voice_1,
    # )
    # # print(tree.insert_chords(tree.tree))
    # tree = measure.Tree(
    #     voice_1,
    # )

    measures = [
        measure.make_rtm_tree(durations, Fraction(1, 1)),
        measure.make_rtm_tree(durations, Fraction(1, 1)),
        measure.make_rtm_tree(durations, Fraction(1, 1)),
    ]

    score_objects = layout.create_score_objects()
    # print(score_objects)

    # TODO: segments
    sections = [
        {
            'voice_1': voice_1,
            'voice_3': voice_2,
            'voice_5': voice_3,
        }
    ]

    # for voice in score_objects.voices:
    #     print(voice.name)

    for m in measure.insert_events(voice_1, measures):
        utils.print_(m)

    voice = Voice()

    for section in sections:
        for voice in score_objects.voices:
            input_ = section.get(voice.name)
            # print(input_)

            container = Container()
            for m in measure.insert_events(input_, measures):
                to_abjad(container, m)
            chords = list(topleveltools.iterate(container).by_class(AbjadChord))
            for group_ in (get_tie_groups(chords)):
                topleveltools.attach(spannertools.Tie(), group_)
            voice.append(container)

    # topleveltools.show(voice)

    # print(voice._)
    topleveltools.show(score_objects.score)

    # print('*********')
    # print(pp.pprint(voice))

    # print(pp.pprint(measure.insert_chords(
    #     ['a', 'b', 'c'],
    #     measure.make_rtm_tree(durations, 1),
    # )))



if __name__ == '__main__':

    # def asdf(a, b):

    #     c = a + 1000
    #     def inner(b):
    #         return c + b

    #     return inner(b)

    # pp.pprint(asdf(1, 1))
    main()
