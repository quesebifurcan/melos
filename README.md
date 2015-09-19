**Melos**
=============

Status
-------------
Pre-alpha; expect everything to change.

What is it?
-------------
A library of Clojure functions with which a user can compose a piece of music. 

It is deliberately idiosyncratic and not intended as a general toolkit for composition.

That said -- if you happen to share some of my musical interests (gradually evolving musics, counterpoint, microtonality), you might find something you can use.

Program flow
-------------
Sequences of musical events (Notes, Chords etc.) are defined, using standard Clojure data structures. 

These sequences roughly correspond to musical voices. 

The voices are then interleaved, resulting in a simple hocket (https://en.wikipedia.org/wiki/Hocket).

The hocket is sliced vertically and a function which calculates the relative dissonance of a step is applied to decide which events can be extended (and how). 

Goal: given a set of independent voices and a maximum allowed level of relative dissonance, extend as many events for as long as possible.

A Python script then translates the result into a Lilypond file, which can be rendered as PDF or MIDI. This part leverages **Abjad** http://abjad.mbrsi.org/.
