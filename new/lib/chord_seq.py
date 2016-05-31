import itertools

from . import utils

def cycle_event_seqs(seq, events):
    events = {k: itertools.cycle(v) for k, v in events.items()}
    result = []
    for x in seq:
        result.append(next(events.get(x)))
    return result

def forward_time(chord):
    events = []
    for event in chord.events:
        events.append(utils.update(event, 'count', lambda x: x + 1))
    chord = chord._replace(events=events)
    return chord

def merge_chords(a, b):
    new_parts = {event.part for event in b.events}
    events = []
    for event in a.events:
        if not event.part in new_parts:
            event_ = utils.update(event, 'count', lambda x: x + 1)
            events.append(event_)
    return utils.update(b, 'events', lambda x: x + events)

