SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/env/bin/activate

python $SCRIPT_DIR/main.py \
       --input $SCRIPT_DIR/../output/example_1.json \
       --output $SCRIPT_DIR/../output/example_1.pdf

