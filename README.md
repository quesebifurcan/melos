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

Installation / Usage
-------------
Clone the repository:
```
$ git clone git@github.com:quesebifurcan/melos.git
```
Install python dependencies...
```
$ cd melos
$ virtualenv env
$ source env/bin/activate
$ pip install -r requirements.txt
```
...and make a directory for the files we are about to create:
```
$ mkdir output
```
After that, we should be able to start a Clojure REPL:
```
$ cd clj/melos
$ lein repl
```
This will trigger the installation of all Clojure dependencies (this might take a while).

In the `melos.score.main` namespace, there is a function `render` which allows us to inspect the results. Once the REPL has started, we can render an example:
```
user=> (require '[melos.score.main :refer [render]])
user=> (render)
Rendered PDF to ../../output/score.pdf
nil
```
The `render` function is quite limited in scope; it only renders the output of `melos.score.main/make-score`.

If you are interested in e.g. concatenating several score files, take a look at `scripts/run.sh`. This file includes an example of a different workflow, using [grench](http://leiningen.org/grench.html) to get around the slow startup times of the Clojure REPL. Running a script like this will also give you proper python tracebacks (which is hard to get when shelling out from Clojure, like `render` does).
