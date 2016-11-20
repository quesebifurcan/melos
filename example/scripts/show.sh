SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $SCRIPT_DIR/..

source $SCRIPT_DIR/env/bin/activate

rm $SCRIPT_DIR/../output/example_1.ly

scripts/env/bin/python $SCRIPT_DIR/main.py \
       --input $SCRIPT_DIR/../$1 \
       --output $SCRIPT_DIR/../output/example_1.ly

lilypond --output=$SCRIPT_DIR/../output/example_1 \
         $SCRIPT_DIR/../output/example_1.ly

open $SCRIPT_DIR/../output/example_1.pdf

