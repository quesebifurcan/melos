set -e

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
grench eval "
 (require '[melos.score.main])
 (time (melos.score.main/render))
"
# (time (melos.score.main/make-score \"$SECTION_1\"))

# cd $REPO_DIR
# echo "${green}Building score with Abjad...${normal}"
# source $PY_VIRTUALENV && \
#     cat $SECTION_1 | python py/main.py "asdf"

# echo "${green}Generated score in $PDF_OUTPUT${normal}"
# # open $PDF_OUTPUT

# # source $PY_VIRTUALENV && \
# #     ipython py/midi_output.py -- \
# #             --title "Test" \
# #             --author "Anonymous" \
# #             --score-out $PDF_OUTPUT \
# #             --input-file \
# #             $SECTION_1

# open $PDF_OUTPUT
