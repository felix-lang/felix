pdflatex -shell-escape src/docs/felix-ref.tex
pdflatex -shell-escape src/docs/felix-ref.tex
makeindex felix-ref
makeglossaries felix-ref
pdflatex -shell-escape src/docs/felix-ref.tex
cp felix-ref.pdf ~/Desktop
#
pdflatex -shell-escape src/docs/technote-compact-linear-types.tex
pdflatex -shell-escape src/docs/technote-compact-linear-types.tex
cp technote-compact-linear-types.pdf ~/Desktop
#
pdflatex -shell-escape src/docs/technote-row-polymorphism.tex
pdflatex -shell-escape src/docs/technote-row-polymorphism.tex
cp technote-row-polymorphism.pdf ~/Desktop
#
pdflatex -shell-escape src/docs/ci.tex
pdflatex -shell-escape src/docs/ci.tex
cp ci.pdf ~/Desktop

pdflatex -shell-escape src/docs/coroutines.tex
pdflatex -shell-escape src/docs/coroutines.tex
cp coroutines.pdf ~/Desktop
#
pdflatex -shell-escape src/docs/modern_programming.tex
pdflatex -shell-escape src/docs/modern_programming.tex
makeindex modern_programming
makeglossaries modern_programming
pdflatex -shell-escape src/docs/modern_programming.tex
cp modern_programming.pdf ~/Desktop


