#! /bin/bash

echo '\usepackage{pgfpages}\setbeameroption{show notes on second screen=right}' > with-notes.txt

echo '\setbeameroption{hide notes}' > slides-only.txt

pandoc -s -i -t beamer --variable fontsize=10pt --include-in-header slides-only.txt presentation.md -o presentation.pdf

pandoc -s -i -t beamer --variable fontsize=10pt --include-in-header with-notes.txt presentation.md -o presentation-notes.pdf

rm slides-only.txt
rm with-notes.txt
