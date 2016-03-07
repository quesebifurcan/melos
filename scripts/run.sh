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

grench eval "(require '[clojure.tools.namespace.repl :refer [refresh]]) (refresh)"
grench eval "(time (melos.score.main/-main \"$JSON_OUTPUT\"))"
grench eval "(time (melos.score.asdf/-main \"$REPO_DIR/output/part_2.json\"))"

cd $REPO_DIR

echo "${red}\nABJAD...${normal}"
source $PY_VIRTUALENV && \
    ipython py/main.py -- \
           --title "Test" \
           --author "Anonymous" \
           --score-out $PDF_OUTPUT \
           --midi-out $MIDI_OUTPUT \
           --input-files $JSON_OUTPUT $REPO_DIR/output/part_2.json
