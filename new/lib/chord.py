import itertools
from . import utils

def select_chord_key(k, chord):
    return (getattr(event, k) for event in chord.events)

def remove_parts(part_names, chord):
    result = []
    for event in chord.events:
        if not event.part in part_names:
            result.append(event)
    return utils.assoc(chord, 'events', result)

def pitch_to_pitchclass(p):
    if not (-60 <= p <= 60):
        raise Exception('pitch {} out of range'.format(p))
    return (p + 60) % 12

def inversion_equivalent_pitchclass(pc):
    if pc > 6:
        return 12 - pc
    return pc

def all_intervals(pitches):
    pcs = map(pitch_to_pitchclass, pitches)
    return set(itertools.combinations(pcs, 2))

def dissonance_value(mapping, intervals):
    intervals = map(inversion_equivalent_pitchclass, intervals)
    result = 0
    for interval in intervals:
        result += mapping.get(interval)
    return result

def pitch_tuple_to_interval(t):
    a, b = t
    return abs(a - b)

def scaled_dissonance_value(mapping, pitches):
    intervals = [pitch_tuple_to_interval(p) for p in all_intervals(pitches)]
    if len(intervals) < 1:
        return 0
    else:
        return dissonance_value(mapping, intervals) / len(intervals)

def is_consonant(mapping, limit, pitches):
    return (scaled_dissonance_value(mapping, pitches) <=
            scaled_dissonance_value(mapping, limit))
