import inspect

from abjad.tools.scoretools import (
    Note as AbjadNote,
    Rest as AbjadRest,
    Chord as AbjadChord,
    Measure as AbjadMeasure,
    Voice as AbjadVoice,
    Staff as AbjadStaff,
    Score as AbjadScore,

    Container,
    FixedDurationTuplet,
)
from abjad.tools.durationtools import (
    Duration as AbjadDuration,
)
from abjad.tools.topleveltools import (
    show,
)

from layout import (
    create_score_objects
)

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
            self.asdf = init(self, data)
        else:
            self.asdf = None
    def get_params(self):
        if inspect.ismethod(self.params):
            return self.params().items()
        else:
            return self.params.items()
    def to_abjad(self):
        return self.asdf

class Many(Converter):
    def __init__(self, xs):
        result = []
        for x in xs:
            result.append(self.root(x).to_abjad())
        self.asdf = result

class Note(Converter):
    params = {
        'pitch': identity,
    }
    def to_abjad(self):
        return self

class Notes(Many):
    root = Note

class Chord(Converter):
    params = {
        'duration': identity,
        'tempo': identity,
        'phrase_end_bool': identity,
        'events': Notes,
    }
    def to_abjad(self):
        if self.asdf and self.asdf.events:
            pitches = [x.asdf.pitch for x in self.asdf.events]
            return AbjadChord(pitches, AbjadDuration(1, 4))
        elif self.asdf:
            return AbjadRest(AbjadDuration(1, 4))

class RhythmTreeNode(Converter):
    # 1. is top level?
    # 2. has scaled duration?
    def params(self):
        return {
            'duration': identity,
            'children': RhythmTreeNodes,
            'chord': Chord
        }
    def to_abjad(self):
        if self.asdf.chord:
            chord = self.asdf.chord
            chord.written_duration = self.asdf.duration
            return chord
        else:
            container = FixedDurationTuplet(self.asdf.duration, [])
            container.extend(self.asdf.children)
            return container

        # return (
        #     self.asdf.duration,
        #     self.asdf.children,
        #     self.asdf.chord,
        # )

class RhythmTreeNodes(Many):
    root = RhythmTreeNode

from fractions import Fraction

class Measure(Converter):
    params = {
        'duration': identity,
        'children': RhythmTreeNodes,
    }
    def to_abjad(self):
        result = AbjadMeasure((1, 1))
        tuplet = FixedDurationTuplet((1, 1), [])
        result.append(tuplet)
        for node in self.asdf.children:
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
        result = AbjadVoice(name=self.asdf.name)
        for measure in self.asdf.measures:
            result.append(measure)
        return result

class Voices(Many):
    root = Voice

class Section(Converter):
    params = {
        'voices': Voices
    }
    def to_abjad(self):
        return self.asdf.voices

class Sections(Many):
    root = Section

def templates():
    return {
        'asdf': create_score_objects,
    }

class Score(Converter):
    params = {
        'title': identity,
        'score_template': identity,
        'sections': Sections
    }
    def get_template(self):
        return templates().get(self.asdf.score_template)()

    def to_abjad(self):

        score_data = self.get_template()

        for section in self.asdf.sections:
            containers = {}
            for k in score_data.staves.keys():
                containers[k] = Container(name=k, is_simultaneous=True)
            for voice in section:
                staff = score_data.staff_to_voices_mapping[voice.name]
                containers[staff].append(voice)
                print(voice)
            for k, v in containers.items():
                score_data.staves[k].append(v)

            #     # score_data.staves[staff['name']].append(staff['container'])
            #     # print(voice.name)
            #     print(voice)

        show(score_data.score)
        # for staff in score_data.staves:
        #     print(staff.name)
        import sys; sys.exit()

        # for section in self.asdf.sections:

        return ('score:', self.asdf.__dict__)

def convert(score):
    print(Score(score).to_abjad())
    # print(Score(score).result.sections)
    # print(Score(score).result.title)
    return 'Score'

