pdflatex -shell-escape src/docs/felix-ref.tex
pdflatex -shell-escape src/docs/felix-ref.tex
makeindex felix-ref
makeglossaries felix-ref
pdflatex -shell-escape src/docs/felix-ref.tex
cp felix-ref.pdf ~/Desktop
pdflatex -shell-escape src/docs/technote-compact-linear-types.tex
pdflatex -shell-escape src/docs/technote-compact-linear-types.tex
cp technote-compact-linear-types.pdf ~/Desktop
pdflatex -shell-escape src/docs/technote-row-polymorphism.tex
pdflatex -shell-escape src/docs/technote-row-polymorphism.tex
cp technote-row-polymorphism.pdf ~/Desktop


