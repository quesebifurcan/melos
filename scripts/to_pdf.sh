SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $SCRIPT_DIR/..

source $SCRIPT_DIR/env/bin/activate

python $SCRIPT_DIR/run.py \
       --input $SCRIPT_DIR/../$1 \
       --output $SCRIPT_DIR/../output/example_1.pdf

