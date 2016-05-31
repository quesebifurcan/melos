from collections import namedtuple, Iterable
from fractions import Fraction
import pprint

from new.lib import (
    schemas,
    utils,
)

def get_duration(node):
    if isinstance(node, Iterable):
        return node[0]
    else:
        return node

def get_summed_durations(template, node):
    children = template.get(node)
    if children:
        coll = []
        for child in children:
            coll.append(get_summed_durations(template, child))
        return sum(coll)
    else:
        return get_duration(node)

def make_rtm_tree(template, node):
    nxt = template.get(node)
    children = []
    if nxt:
        for child in nxt:
            children.append(make_rtm_tree(template, child))
    return schemas.RhythmTreeNode(
        duration=get_duration(node),
        chord=None,
        sum_of_leaves_duration=get_summed_durations(template, get_duration(node)),
        children=children,
    )

class InsertChords:

    def __init__(self, notes):
        self.notes = notes

    def insert_chords(self, node):
        if isinstance(node, list):
            result = []
            for measure in node:
                result.append(self.insert_chords(measure))
            return result
        if not self.notes:
            node = utils.assoc(node, 'chord', 'rest')
            children = []
            for child in node.children:
                children.append(self.insert_chords(child))
            node = utils.assoc(node, 'children',  [])
            return node
        elif self.notes[0].duration > node.sum_of_leaves_duration:
            reduced_dur = self.notes[0].duration - node.sum_of_leaves_duration
            node = utils.assoc(node, 'chord',  self.notes[0])
            node = utils.assoc(node, 'children',  [])
            self.notes = (
                [utils.assoc(self.notes[0], 'duration', reduced_dur)] +
                self.notes[1:]
            )
            return node
        elif self.notes[0].duration == node.sum_of_leaves_duration:
            node = utils.assoc(node, 'chord',  self.notes[0])
            node = utils.assoc(node, 'children',  [])
            self.notes = self.notes[1:]
            return node
        else:
            children = []
            for child in node.children:
                children.append(self.insert_chords(child))
            node = utils.assoc(node, 'children',  children)
            return node

def insert_events(events, measures):
    instance = InsertChords(events)
    return instance.insert_chords(measures)
