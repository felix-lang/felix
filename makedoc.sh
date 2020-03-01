cd docs
pdflatex -shell-escape ../src/docs/architecture.tex
pdflatex -shell-escape ../src/docs/architecture.tex
mv architecture.pdf ../pdfs
#
pdflatex -shell-escape ../src/docs/felix-ref.tex
pdflatex -shell-escape ../src/docs/felix-ref.tex
makeindex felix-ref
makeglossaries felix-ref
pdflatex -shell-escape ../src/docs/felix-ref.tex
mv felix-ref.pdf ../pdfs
#
pdflatex -shell-escape ../src/docs/technote-compact-linear-types.tex
pdflatex -shell-escape ../src/docs/technote-compact-linear-types.tex
mv technote-compact-linear-types.pdf ../pdfs
#
pdflatex -shell-escape ../src/docs/technote-row-polymorphism.tex
pdflatex -shell-escape ../src/docs/technote-row-polymorphism.tex
mv technote-row-polymorphism.pdf ../pdfs
#
pdflatex -shell-escape ../src/docs/ci.tex
pdflatex -shell-escape ../src/docs/ci.tex
mv ci.pdf ../pdfs

pdflatex -shell-escape ../src/docs/coroutines.tex
pdflatex -shell-escape ../src/docs/coroutines.tex
mv coroutines.pdf ../pdfs
#
pdflatex -shell-escape ../src/docs/modern_programming.tex
pdflatex -shell-escape ../src/docs/modern_programming.tex
makeindex modern_programming
makeglossaries modern_programming
pdflatex -shell-escape ../src/docs/modern_programming.tex
mv modern_programming.pdf ../pdfs

#
pdflatex -shell-escape ../src/docs/coprogramming.tex
pdflatex -shell-escape ../src/docs/coprogramming.tex
makeindex coprogramming
makeglossaries coprogramming
pdflatex -shell-escape ../src/docs/coprogramming.tex
mv coprogramming.pdf ../pdfs
cd ..
sh parsedoc.sh

