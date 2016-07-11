import inspect

from fractions import Fraction

from abjad.tools import scoretools
from abjad.tools import durationtools
from abjad.tools import topleveltools
from abjad.tools import indicatortools
from abjad.tools import spannertools

def get(d, k):
    return d.get(k)

def duration_to_duration_tuple(dur):
    return {
        0.125: (1, 8),
        0.25:  (1, 4),
        0.5:   (1, 2),
        0.75:  (3, 4),
        1:     (4, 4),
    }.get(dur)

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

def init(self, data):
    result = type('temp', (object,), {})()
    for k, v in self.get_params():
        if inspect.isclass(v):
            converted_value = v(get(data, k)).to_abjad()
            setattr(result, k, converted_value)
        elif inspect.isfunction(v):
            converted_value = v(get(data, k))
            setattr(result, k, converted_value)
        else:
            raise Exception("Invalid converter for {}: {}".format(self, v))
    return result

class Converter:
    def __init__(self, data):
        if data:
            self.converted = init(self, data)
        else:
            self.converted = None
    def get_params(self):
        if inspect.ismethod(self.params):
            return self.params().items()
        else:
            return self.params.items()
    def to_abjad(self):
        return self.converted

class Many(Converter):
    def __init__(self, xs):
        result = []
        for x in xs:
            result.append(self.root(x).to_abjad())
        self.converted = result

class Note(Converter):
    params = {
        'pitch': identity,
        'group': identity,
        'notation': identity,
        'is-rest?': identity,
    }
    def to_abjad(self):
        return self

class Notes(Many):
    root = Note

def collect_notations(events):
    notations = [x.converted.notation for x in events]
    result = {}
    for notation in notations:
        if notation:
            for k, v in notation.items():
                result[k] = v
    return result

class Chord(Converter):
    params = {
        'duration': duration_to_duration_tuple,
        'tempo': identity,
        'phrase-end?': identity,
        'events': Notes,
    }
    def to_abjad(self):
        if self.converted and self.converted.events:
            rests = [getattr(x.converted, 'is-rest?') for x in self.converted.events]
            if any(rests):
                return scoretools.Rest(durationtools.Duration(1, 4))
            pitches = [x.converted.pitch for x in self.converted.events]
            groups = [x.converted.group for x in self.converted.events]
            groups_annotation = indicatortools.Annotation('groups', groups)
            notations_dict = collect_notations(self.converted.events)
            notations_annotation = indicatortools.Annotation('notation', notations_dict)
            chord = scoretools.Chord(pitches, durationtools.Duration(1, 4))
            topleveltools.attach(notations_annotation, chord)
            topleveltools.attach(groups_annotation, chord)
            return chord
        elif self.converted:
            return scoretools.Rest(durationtools.Duration(1, 4))

class RhythmTreeNode(Converter):
    def params(self):
        return {
            'duration': duration_to_duration_tuple,
            'children': RhythmTreeNodes,
            'chord': Chord
        }
    def to_abjad(self):
        if self.converted.chord:
            chord = self.converted.chord
            chord.written_duration = durationtools.Duration(*self.converted.duration)
            return chord
        else:
            container = scoretools.FixedDurationTuplet(
                durationtools.Duration(*self.converted.duration),
                [])
            container.extend(self.converted.children)
            return container

class RhythmTreeNodes(Many):
    root = RhythmTreeNode

class Measure(Converter):
    params = {
        'chord': Chord,
        'duration': duration_to_duration_tuple,
        'children': RhythmTreeNodes,
    }
    def to_abjad(self):
        result = scoretools.Measure(self.converted.duration)
        tuplet = scoretools.FixedDurationTuplet(self.converted.duration, [])
        result.append(tuplet)
        if self.converted.children:
            for node in self.converted.children:
                tuplet.append(node)
        else:
            chord = self.converted.chord
            chord.written_duration = durationtools.Duration(*self.converted.duration)
            tuplet.append(chord)
        return result

class Measures(Many):
    root = Measure

class Voice(Converter):
    params = {
        'name': identity,
        'measures': Measures,
    }
    def to_abjad(self):
        result = scoretools.Voice()
        for measure in self.converted.measures:
            result.append(measure)
        annotate(result, 'score_id', 'Voice')
        annotate(result, 'name', self.converted.name)
        return result

class Voices(Many):
    root = Voice

class Staff(Converter):
    params = {
        'voices': Voices,
        'name': identity,
        'notation': identity,
        'tempo': identity,
    }
    def to_abjad(self):
        container = scoretools.Container(is_simultaneous=True)
        annotate(container, 'score_id', 'section_container')
        annotate(container, 'name', self.converted.name)
        annotate(container, 'notation', self.converted.notation)
        annotate(container, 'tempo', self.converted.tempo)
        if len(self.converted.voices) > 1:
            topleveltools.override(self.converted.voices[0]).stem.direction = 'up'
            topleveltools.override(self.converted.voices[0]).tuplet_bracket.direction = 'up'
            topleveltools.override(self.converted.voices[0]).tie.direction = 'up'
            topleveltools.override(self.converted.voices[0]).rest.staff_position = 6

            topleveltools.override(self.converted.voices[1]).stem.direction = 'down'
            topleveltools.override(self.converted.voices[1]).tuplet_bracket.direction = 'down'
            topleveltools.override(self.converted.voices[1]).tie.direction = 'down'
            topleveltools.override(self.converted.voices[1]).rest.staff_position = -8
        container.extend(self.converted.voices)
        return container

class Staves(Many):
    root = Staff

class Section(Converter):
    params = {
        'staves': Staves,
    }
    def to_abjad(self):
        return self.converted.staves

class Sections(Many):
    root = Section

class Score(Converter):
    params = {
        'title': identity,
        'score_template': identity,
        'sections': Sections
    }
    def get_template(self):
        return templates().get(self.converted.score_template)()
    def to_abjad(self, template):
        score_data = template
        for section in self.converted.sections:
            for staff_container in section:
                container_name = get_named_annotation(staff_container, 'name')
                score_data.staves[container_name].append(staff_container)
                chords = topleveltools.iterate(staff_container).by_class((scoretools.Chord, scoretools.Rest))
                for group_ in (get_tie_groups(chords)):
                    group__ = list(group_)
                    if isinstance(group_[0], scoretools.Chord):
                        topleveltools.attach(spannertools.Tie(), group_)
        return score_data.score
