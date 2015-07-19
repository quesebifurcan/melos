#!/bin/bash

set -e
set -u

TITLE=$1
AUTHOR=$2

REPO_DIR=$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)
CLJ_DIR=$REPO_DIR/clj/melos
PY_DIR=$REPO_DIR/py
OUTPUT_DIR=$REPO_DIR/output
JSON_SCORE_FILE_NAME=$OUTPUT_DIR/output.json

mkdir -p $OUTPUT_DIR
cd $CLJ_DIR
lein run $JSON_SCORE_FILE_NAME

cd $PY_DIR
ipython main.py \
        $JSON_SCORE_FILE_NAME \
        $TITLE \
        $AUTHOR \
        $OUTPUT_DIR/output.pdf \
        $OUTPUT_DIR/output.mid

# TODO: Fix title page.
# cd $OUTPUT_DIR
# pdflatex ../latex/title_page.tex
# gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=map.pdf ./title_page.pdf ./output.pdf
