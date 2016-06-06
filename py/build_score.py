import itertools

from fractions import Fraction

from abjad import *
from abjad.tools.scoretools import FixedDurationTuplet

def is_tuplet(d):
    return not d.get('written-duration') == d.get('duration')

def is_leaf(node):
    return node.get('events') is not None

def annotate(node, event):
    events = node.get('chord').get('events')
    part = ''
    for e in events:
        part = e.get('part')
    groups = [e.get('group') for e in events]
    notation = [e.get('notation') for e in events]
    pitches = [e.get('pitch') for e in events]
    pitchgroups = {}
    for pitch, group in zip(pitches, groups):
        pitchgroups[pitch] = group
    part_annotation = indicatortools.Annotation('part', part)
    groups_annotation = indicatortools.Annotation('groups', pitchgroups)
    # notation_annotation = indicatortools.Annotation('notation', notation[0])
    attach(part_annotation, event)
    attach(groups_annotation, event)
    # attach(notation_annotation, event)
    return event

def make_note(node):
    duration = Fraction(node.get('duration'))
    events = node.get('chord').get('events')
    pitches = [event.get('pitch') for event in events]
    if not events:
        event = Rest(Duration(duration))
    else:
        event = Chord(pitches, Duration(duration))
    event = annotate(node, event)
    return event

def make_measure(node):
    return Measure(Duration(node.get('duration')))

def make_tuplet(d):
    return FixedDurationTuplet(Duration(Fraction(d.get('duration'))), [])

def extend_node(node, children, is_measure=False, is_tuplet=False):
    if is_measure and is_tuplet:
        measure = make_measure(node)
        tuplet = make_tuplet(node)
        tuplet.extend(children)
        measure.append(tuplet)
        return measure
    if is_measure and not is_tuplet:
        measure = make_measure(node)
        measure.extend(children)
        return measure
    if not is_measure:
        tuplet = make_tuplet(node)
        tuplet.extend(children)
        return tuplet

def parse_node(node, is_measure=False):
    if is_leaf(node):
        note = make_note(node)
        return note
    else:
        children = []
        for child in node.get('children', []):
            if child.get('chord') is not None:
                note = make_note(child)
                children.append(note)
            else:
                children.append(parse_node(child))
        return extend_node(node, children, is_measure, is_tuplet(node))

def parse_segment(segment):
    result = {}
    section_name = segment.get('section-name')
    voices = segment.get('voices')
    for part_name in segment.get('parts', []):
        rhythmTreeNodes = voices.get(part_name).get('measures')
        measures = []
        for node in rhythmTreeNodes:
            measure = parse_node(node, is_measure=True)
            measures.append(measure)
        result[part_name] = measures
    return result

def parse_segments(score_segments, voices):
    for i, segment in enumerate(score_segments):
        music = parse_segment(segment)
        for voice in voices:
            try:
                measures = music[voice.name]
            except KeyError:
                print('No music for "{}" was found, skipping'.format(voice.name))
                continue
            voice.extend(measures)
