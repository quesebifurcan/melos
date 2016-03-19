set -e

red=$(tput setaf 1)
normal=$(tput sgr0)

REPO_DIR=$(cd "$( dirname "${BASH_SOURCE[0]}" )" && cd ./../ && pwd)
CLOJURE_DIR=$REPO_DIR/clj/melos

source $REPO_DIR/scripts/defaults.cfg
mkdir -p $ANALYSIS_DIR
mkdir -p $OUTPUT_DIR

# Run in separate terminal window:
# lein trampoline repl :headless

cd $CLOJURE_DIR

echo "${red}\nReloading namespaces...${normal}"

SECTION_1=$REPO_DIR/output/section_1.json
SECTION_2=$REPO_DIR/output/section_2.json

grench eval "(require '[clojure.tools.namespace.repl :refer [refresh]]) (refresh)"
grench eval "(time (melos.score.main/open \"$SECTION_1\"))"
grench eval "(time (melos.score.main/closed \"$SECTION_2\"))"

cd $REPO_DIR

echo "${red}\nABJAD...${normal}"
source $PY_VIRTUALENV && \
    ipython py/main.py -- \
           --title "Test" \
           --author "Anonymous" \
           --score-out $PDF_OUTPUT \
           --midi-out $MIDI_OUTPUT \
           --input-files \
           $SECTION_1 \
           $SECTION_2

open $PDF_OUTPUT
