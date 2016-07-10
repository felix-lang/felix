pdflatex -shell-escape src/docs/felix-ref.tex
pdflatex -shell-escape src/docs/felix-ref.tex
makeindex felix-ref
makeglossaries felix-ref
pdflatex -shell-escape src/docs/felix-ref.tex
cp felix-ref.pdf ~/Desktop
