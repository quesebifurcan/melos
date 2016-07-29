import inspect
import itertools

from functools import partial
from fractions import Fraction

from abjad.tools import scoretools
from abjad.tools import durationtools
from abjad.tools import topleveltools
from abjad.tools import indicatortools
from abjad.tools import spannertools

def float_to_duration(dur):
    return durationtools.Duration(dur).with_denominator(4)

def identity(x): return x

def annotate(obj, key, value):
    topleveltools.attach(indicatortools.Annotation(key, value), obj)
    return obj

def get_named_annotation(obj, name):
    annotations = topleveltools.inspect_(obj).get_indicators(indicatortools.Annotation)
    for annotation in annotations:
        if annotation.name == name:
            return annotation.value
    return None

def contains_any(a, b):
    if a and b:
        return set(a).intersection(set(b))
    return False

def collect_notations(events):
    notations = [x.notation for x in events]
    result = {}
    for notation in notations:
        if notation:
            for k, v in notation.items():
                result[k] = v
    return result

class groupby:
    # Adds custom equality function to itertools.groupby
    def __init__(self, iterable, key=None, eqcheck=None):
        if key is None:
            key = lambda x: x
        self.keyfunc = key
        if not eqcheck:
            self.eqfunc = lambda a, b: a == b
        else:
            self.eqfunc = eqcheck
        self.it = iter(iterable)
        self.tgtkey = self.currkey = self.currvalue = object()
    def __iter__(self):
        return self
    def __next__(self):
        while self.currkey == self.tgtkey:
            self.currvalue = next(self.it)    # Exit on StopIteration
            self.currkey = self.keyfunc(self.currvalue)
        self.tgtkey = self.currkey
        return (self.currkey, self._grouper(self.tgtkey))
    def _grouper(self, tgtkey):
        while self.eqfunc(self.currkey, tgtkey):
            yield self.currvalue
            self.currvalue = next(self.it)    # Exit on StopIteration
            self.currkey = self.keyfunc(self.currvalue)

def group_events(fn):
    def inner(iterator):
        for k, g in itertools.groupby(iterator, fn):
            if k:
                yield (k, g)
    return inner

def init(self, data):
    for k, v in self.get_params():
        if inspect.isclass(v):
            converted_value = v(data.get(k)).to_abjad()
            setattr(self, k, converted_value)
        elif inspect.isfunction(v):
            converted_value = v(data.get(k))
            setattr(self, k, converted_value)
        else:
            raise Exception("Invalid converter for {}: {}".format(self, v))

class Converter:
    annotations = []
    def __init__(self, data):
        if data:
            init(self, data)
            self.converted = True
        else:
            self.converted = False
    def annotate(self, x):
        if self.annotations:
            for annotation_name in self.annotations:
                annotate(x, annotation_name, getattr(self, annotation_name))
        return x
    def get_params(self):
        if inspect.ismethod(self.params):
            return self.params().items()
        else:
            return self.params.items()
    def to_abjad(self):
        return self

class Many(Converter):
    def __init__(self, xs):
        result = []
        for x in xs:
            result.append(self.root(x).to_abjad())
        self.result = result
        self.converted = True
    def to_abjad(self):
        return self.result

class Note(Converter):
    params = {
        'part': identity,
        'pitch': identity,
        'group': identity,
        'notation': identity,
        'instrument': identity,
        'is-rest?': identity,
    }

class Notes(Many):
    root = Note

class Chord(Converter):
    default_duration = durationtools.Duration((1, 4))
    params = {
        'duration': lambda x: durationtools.Multiplier(x).with_denominator(4),
        'tempo': identity,
        'phrase-end?': identity,
        'events': Notes,
    }
    def to_abjad(self):
        if self.converted and self.events:
            rests = [getattr(x, 'is-rest?') for x in self.events]
            if any(rests):
                return scoretools.Rest(self.default_duration)
            # Pitches
            pitches = [x.pitch for x in self.events]
            chord = scoretools.Chord(pitches, self.default_duration)
            # Groups
            groups = [x.group for x in self.events]
            groups_annotation = indicatortools.Annotation('groups', groups)
            topleveltools.attach(groups_annotation, chord)
            # Parts
            part = [x.part for x in self.events][0]
            part_annotation = indicatortools.Annotation('part', part)
            topleveltools.attach(part_annotation, chord)
            # Instrument
            instrument = [x.instrument for x in self.events][0]
            instrument_annotation = indicatortools.Annotation('instrument', instrument)
            topleveltools.attach(instrument_annotation, chord)
            # Notations
            notations = [x.notation for x in self.events]
            if all(notations):
                notations_dict = notations[0]
            else:
                notations_dict = {}
            notations_annotation = indicatortools.Annotation(
                'notation', notations_dict
            )
            topleveltools.attach(notations_annotation, chord)
            return chord
        elif self.converted:
            return scoretools.Rest(durationtools.Duration(1, 4))

class RhythmTreeNode(Converter):
    def params(self):
        return {
            'duration': float_to_duration,
            'children': RhythmTreeNodes,
            'chord': Chord
        }
    def to_abjad(self):
        if self.chord:
            chord = self.chord
            chord.written_duration = self.duration
            return chord
        else:
            container = scoretools.FixedDurationTuplet(self.duration, [])
            container.extend(self.children)
            return container

class RhythmTreeNodes(Many):
    root = RhythmTreeNode

class Measure(Converter):
    params = {
        'chord': Chord,
        'duration': float_to_duration,
        'children': RhythmTreeNodes,
    }
    def to_abjad(self):
        result = scoretools.Measure(self.duration)
        tuplet = scoretools.FixedDurationTuplet(self.duration, [])
        result.append(tuplet)
        if self.children:
            for node in self.children:
                tuplet.append(node)
        else:
            chord = self.chord
            chord.written_duration = self.duration
            tuplet.append(chord)
        return result

class Measures(Many):
    root = Measure

class Voice(Converter):
    params = {
        'name': identity,
        'measures': Measures,
    }
    score_id = 'Voice'
    annotations = (
        'score_id',
        'name',
    )
    def to_abjad(self):
        result = scoretools.Voice()
        for measure in self.measures:
            result.append(measure)
        return self.annotate(result)

class Voices(Many):
    root = Voice

class Staff(Converter):
    params = {
        'voices': Voices,
        'name': identity,
        'notation': identity,
        'tempo': identity,
    }
    score_id = 'section_container'
    annotations = (
        'score_id',
        'name',
        'notation',
        'tempo',
    )
    def apply_overrides(self):
        if len(self.voices) > 1:
            topleveltools.override(self.voices[0]).stem.direction = 'up'
            topleveltools.override(self.voices[1]).stem.direction = 'down'
            topleveltools.override(self.voices[0]).tuplet_bracket.direction = 'up'
            topleveltools.override(self.voices[1]).tuplet_bracket.direction = 'down'
            topleveltools.override(self.voices[0]).tie.direction = 'up'
            topleveltools.override(self.voices[1]).tie.direction = 'down'
            topleveltools.override(self.voices[0]).rest.staff_position = 6
            topleveltools.override(self.voices[1]).rest.staff_position = -8
    def to_abjad(self):
        result = scoretools.Container(is_simultaneous=True)
        result.extend(self.voices)
        self.annotate(result)
        self.apply_overrides()
        return result

class Staves(Many):
    root = Staff

class Section(Converter):
    params = {
        'staves': Staves,
    }
    def to_abjad(self):
        return self.staves

class Sections(Many):
    root = Section

class NotationSpanner(spannertools.Spanner):
    def __init__(self, key='', value=None):
        super().__init__()
        self.key = key
        self.value = value

def is_tied(a, b):
    a_ = get_named_annotation(a, 'groups')
    b_ = get_named_annotation(b, 'groups')
    if a_ and b_:
        return any([x in b_ for x in a_])
    return False

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

class Score(Converter):
    params = {
        'title': identity,
        'score_template': identity,
        'sections': Sections
    }
    def get_template(self):
        return templates().get(self.score_template)()
    def apply_spanners(self, score):
        spanner_groups = {
            # 'groups': lambda score: groupby(
            #     score,
            #     key=lambda x: get_named_annotation(x, 'groups'),
            #     eqcheck=contains_any,
            # ),
            'notation': group_events(
                lambda x: get_named_annotation(x, 'notation'),
            ),
            'instrument': group_events(
                lambda x: (
                    get_named_annotation(x, 'part'),
                    get_named_annotation(x, 'instrument'),
                )
            ),
        }
        # groups
        classes = (scoretools.Chord, scoretools.Rest)
        chords_iterator = list(topleveltools.iterate(score).by_class(classes))
        tie_groups = get_tie_groups(list(chords_iterator))
        for tie_group in tie_groups:
            if isinstance(tie_group[0], scoretools.Chord):
                spanner = NotationSpanner(key='groups', value=True)
                topleveltools.attach(spanner, tie_group)
        # notation, instrument
        for spanner_name, group_fn in spanner_groups.items():
            classes = (scoretools.Chord, scoretools.Rest)
            chords_iterator = topleveltools.iterate(score).by_class(classes)
            for k, g in group_fn(chords_iterator):
                group = list(g)
                if k and isinstance(group[0], scoretools.Chord):
                    spanner = NotationSpanner(key=spanner_name, value=k)
                    topleveltools.attach(spanner, group)
    def to_abjad(self):
        return self.sections
        # score_data = template
        # return score_data.score, score_data.staves, self.sections, self.apply_spanners
