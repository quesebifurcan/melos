import inspect

from fractions import Fraction

from abjad.tools import scoretools
from abjad.tools import durationtools
from abjad.tools import topleveltools
from abjad.tools import indicatortools
from abjad.tools import spannertools

from melos.layout import (
    create_score_objects,
    make_lilypond_file,
)

def duration_to_duration_tuple(dur):
    return {
        0.125: (1, 8),
        0.25: (1, 4),
        0.5: (1, 2),
        0.75: (3, 4),
        1: (4, 4),
    }.get(dur)

def templates():
    return {
        'asdf': create_score_objects,
    }

def identity(x): return x

def init(self, data):
    result = type('temp', (object,), {})()
    for k, v in self.get_params():
        if inspect.isclass(v):
            converted_value = v(getattr(data, k)).to_abjad()
            setattr(result, k, converted_value)
        elif inspect.isfunction(v):
            converted_value = v(getattr(data, k))
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
    }
    def to_abjad(self):
        return self

class Notes(Many):
    root = Note

class Chord(Converter):
    params = {
        'duration': duration_to_duration_tuple,
        'tempo': identity,
        'phrase_end_bool': identity,
        'events': Notes,
    }
    def to_abjad(self):
        if self.converted and self.converted.events:
            pitches = [x.converted.pitch for x in self.converted.events]
            groups = [x.converted.group for x in self.converted.events]
            groups_annotation = indicatortools.Annotation('groups', groups)
            chord = scoretools.Chord(pitches, durationtools.Duration(1, 4))
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
        'duration': duration_to_duration_tuple,
        'children': RhythmTreeNodes,
    }
    def to_abjad(self):
        result = scoretools.Measure(self.converted.duration)
        tuplet = scoretools.FixedDurationTuplet(self.converted.duration, [])
        result.append(tuplet)
        for node in self.converted.children:
            tuplet.append(node)
        return result

class Measures(Many):
    root = Measure

class Voice(Converter):
    params = {
        'name': identity,
        'measures': Measures,
    }
    def to_abjad(self):
        result = scoretools.Voice(name=self.converted.name)
        for measure in self.converted.measures:
            result.append(measure)
        return result

class Voices(Many):
    root = Voice

class Section(Converter):
    params = {
        'voices': Voices
    }
    def to_abjad(self):
        return self.converted.voices

class Sections(Many):
    root = Section

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

class Score(Converter):
    params = {
        'title': identity,
        'score_template': identity,
        'sections': Sections
    }
    def get_template(self):
        return templates().get(self.converted.score_template)()
    def to_abjad(self):
        score_data = self.get_template()
        for section in self.converted.sections:
            containers = {}
            for k in score_data.staves.keys():
                containers[k] = scoretools.Container(name=k, is_simultaneous=True)
            for voice in section:
                staff = score_data.staff_to_voices_mapping[voice.name]
                containers[staff].append(voice)
            for k, v in containers.items():
                score_data.staves[k].append(v)
            for voice in section:
                chords = list(topleveltools.iterate(voice).by_class(scoretools.Chord))
                for group_ in (get_tie_groups(chords)):
                    topleveltools.attach(spannertools.Tie(), group_)
        for staff in score_data.staves.values():
            topleveltools.override(staff).time_signature.style = 'numeric'
        return make_lilypond_file(
            score_data.score,
            self.converted.title,
            'tester',
        )

def convert(score):
    return Score(score).to_abjad()
