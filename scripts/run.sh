set -e

# Run in separate terminal window:
# lein trampoline repl :headless

PY_VIRTUALENV=./env/bin/activate
REPO_DIR=$(cd "$( dirname "${BASH_SOURCE[0]}" )" && cd ./../ && pwd)
OUTPUT_DIR=$REPO_DIR/output
CLOJURE_DIR=$REPO_DIR/clj/melos
JSON_OUTPUT=$OUTPUT_DIR/score.json
PDF_OUTPUT=$OUTPUT_DIR/score.pdf
MIDI_OUTPUT=$OUTPUT_DIR/score.midi

green=$(tput setaf 2)
normal=$(tput sgr0)

mkdir -p $OUTPUT_DIR

cd $CLOJURE_DIR
echo "${green}Reloading namespaces...${normal}"
SECTION_1=$REPO_DIR/output/section_1.json
grench eval "(require '[clojure.tools.namespace.repl :refer [refresh]]) (refresh)"
grench eval "(time (melos.score.main/make-score \"$SECTION_1\"))"

cd $REPO_DIR

echo "${green}Building score with Abjad...${normal}"
source $PY_VIRTUALENV && \
    ipython py/main.py -- \
            --title "Test" \
            --author "Anonymous" \
            --score-out $PDF_OUTPUT \
            --input-files \
            $SECTION_1

echo "${green}Generated score in $PDF_OUTPUT${normal}"
