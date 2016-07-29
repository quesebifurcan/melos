import argparse
import itertools
import json

from collections import namedtuple
from termcolor import colored

from abjad import *
from abjad.tools.scoretools import FixedDurationTuplet

# from main import make_score

Event = namedtuple('Event', [
    'pitch',
    'velocity',
    'time',
    'registration',
    'part',
])

def split_chords(staff):

    def get_timespan(elt):
        agent = inspect_(elt)
        return agent.get_timespan(in_seconds=True)

    def get_annotation(obj, name):
        annotations = topleveltools.inspect_(obj).get_indicators(indicatortools.Annotation)
        for annotation in annotations:
            if annotation.name == name:
                return annotation.value
        return None

    coll = []
    pending = {}
    for abjad_obj in iterate(staff).by_class((Chord, Rest)):
        timespan = get_timespan(abjad_obj)
        notation = get_annotation(abjad_obj, 'notation')
        part = get_annotation(abjad_obj, 'part')
        group = get_annotation(abjad_obj, 'groups')
        registration = None
        if notation:
            registration = notation.get('registration')
        start = timespan.start_offset
        stop = timespan.stop_offset
        if isinstance(abjad_obj, Chord):
            old_pitches = list(pending.keys())
            new_pitches = abjad_obj.written_pitches
            # Pop "old" pitches if they are not present
            # in the current abjad_obj (Chord).
            # Add these pitches to "coll".
            for pitch in old_pitches:
                if not pitch in new_pitches:
                    start_event = pending.get(pitch)
                    stop_event = start_event._replace(time=start - 0.1, velocity=0)
                    coll.append(start_event)
                    coll.append(stop_event)
                    pending.pop(pitch, None)
            # Add current pitches to "pending" if they are not present
            # in `old_pitches`
            for pitch in new_pitches:
                if not pitch in old_pitches:
                    new_event = Event(
                        pitch=pitch.pitch_number,
                        velocity=120,
                        time=start,
                        registration=registration,
                        part=part,
                    )
                    pending[pitch] = new_event
        # If we encounter a Rest, collect all pending pitches and add them
        # to "coll". Set their "stop" value to the starting point of this
        # Rest.
        if isinstance(abjad_obj, Rest):
            old_pitches = pending.keys()
            for pitch in old_pitches:
                start_event = pending.get(pitch)
                stop_event = start_event._replace(time=start - 0.1, velocity=0)
                coll.append(start_event)
                coll.append(stop_event)
            pending = {}
    # We might have some pending pitches left at the end; collect them
    # and add them to coll. Set "stop" value to "stop" of the last event.
    old_pitches = pending.keys()
    for pitch in old_pitches:
        start_event = pending.get(pitch)
        stop_event = start_event._replace(time=stop, velocity=0)
        coll.append(start_event)
        coll.append(stop_event)
    return coll

def event_to_qlist_item(event, delta):
    # print(event)
    track_name = '-'.join([event.part, event.registration])
    result = ' '.join([
        str(int(delta * 1000)),
        track_name,
        str(event.pitch + 60),
        str(event.velocity),
    ])
    return result + ';'

def export_as_qlist(score):
    from melos import to_abjad
    all_events = []
    for spanner in iterate(score).by_spanner(to_abjad.NotationSpanner):
        if spanner.key == 'instrument':
            part, instrument = spanner.value
            for tie_chain in iterate(spanner.components).by_logical_tie():
                events = split_chords(tie_chain)
                coll = []
                for event in events:
                    e = event._replace(registration=instrument,
                                        part=part)
                    coll.append(e)
                all_events.extend(coll)
    all_events = sorted(all_events, key=lambda x: x.time)
    curr = 0
    result = []
    for event in all_events:
        delta = event.time - curr
        result.append(event_to_qlist_item(event, delta))
        curr = event.time
    return result
