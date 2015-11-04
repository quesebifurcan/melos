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

grench eval "(melos.utils/export-to-json \"/Users/fred/Desktop/testing.json\"
                (map #(take 10 %) (score.group-a/upper)))"

cd $REPO_DIR

source ~/.envs/melodies/bin/activate && \
    ipython py/partial_run.py \
            ~/Desktop/testing.json \
            "testing" \
            "Wallberg" \
            ~/projects/music/compositions/2015/organ/output/output.pdf \
            ~/Desktop/abcd.midi && \
    mv ~/Desktop/abcd.midi \
       ~/Desktop/abcd.mid && \
    open ~/projects/music/compositions/2015/organ/output/output.pdf
