SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $SCRIPT_DIR/..

source $SCRIPT_DIR/env/bin/activate

rm $SCRIPT_DIR/../output/example_1.ly
rm $SCRIPT_DIR/../output/example_1.pdf

python $SCRIPT_DIR/run.py \
       --input $SCRIPT_DIR/../$1 \
       --output $SCRIPT_DIR/../output/example_1.pdf

pdfunite $SCRIPT_DIR/../output/titlepage.pdf \
         $SCRIPT_DIR/../output/example_1.pdf \
         $SCRIPT_DIR/../output/score.pdf

open $SCRIPT_DIR/../output/score.pdf

