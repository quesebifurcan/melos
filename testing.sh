set -e

red=$(tput setaf 1)
normal=$(tput sgr0)

REPO_DIR=$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)
CLOJURE_DIR=$REPO_DIR/clj/score

# Run in separate terminal window:
# lein trampoline repl :headless

cd $CLOJURE_DIR

echo "${red}\nReloading namespaces...${normal}"
grench eval "(require '[clojure.tools.namespace.repl :refer [refresh]]) (refresh)"

echo "${red}\nCalculating all sessions...${normal}"
grench eval "(time (score.compose-score/calc-all-sessions))"

echo "${red}\nComposing score...${normal}"
grench eval "(time (score.main/-main \"/Users/fred/Desktop/score.json\"))"

cd $REPO_DIR

echo "${red}\nABJAD...${normal}"
source ~/.envs/melodies/bin/activate && \
    ipython py/main.py \
            ~/Desktop/score.json \
            "testing" \
            "Wallberg" \
            ~/projects/music/compositions/2015/organ/output/output.pdf \
            ~/Desktop/abcd.midi && \
    mv ~/Desktop/abcd.midi \
       ~/Desktop/abcd.mid && \
    open ~/projects/music/compositions/2015/organ/output/output.pdf

