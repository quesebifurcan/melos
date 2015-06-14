cd ./clj/melos
lein exec -p src/melos/scores/test_event_seq.clj
cd ../../
ipython py/main.py ~/Desktop/score.json
cd ./output
pdflatex ../latex/title_page.tex
gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=map.pdf ./title_page.pdf ./output.pdf
open ./map.pdf
