set -e

red=$(tput setaf 1)
normal=$(tput sgr0)

REPO_DIR=$(cd "$( dirname "${BASH_SOURCE[0]}" )" && cd ./../ && pwd)
CLOJURE_DIR=$REPO_DIR/clj/melos

source $REPO_DIR/scripts/defaults.cfg
mkdir -p $ANALYSIS_DIR

# Run in separate terminal window:
# lein trampoline repl :headless

cd $CLOJURE_DIR

echo "${red}\nReloading namespaces...${normal}"
grench eval "(require '[clojure.tools.namespace.repl :refer [refresh]]) (refresh)"

if [ "$RECALCULATE_SESSIONS" = true ]; then
    echo "${red}\nCalculating sessions...${normal}"
    for session in ${SESSIONS[@]}
    do
        grench eval "(time (melos.score.compose-score/new-session $session))"
    done
fi

if [ "$RENDER_SCORE" = true ]; then
    echo "${red}\nComposing score...${normal}"
    grench eval "(time (melos.score.main/-main \"$JSON_OUTPUT\" $COMPOSITION_FN [$SESSIONS]))"

    cd $REPO_DIR

    echo "${red}\nABJAD...${normal}"
    source $PY_VIRTUALENV && \
        ipython py/main.py \
                $JSON_OUTPUT \
                $TITLE \
                $AUTHOR \
                $PDF_OUTPUT \
                $MIDI_OUTPUT
fi
