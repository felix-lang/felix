cd tex
pdflatex -shell-escape ../src/tex/typesystem.tex
pdflatex -shell-escape ../src/tex/typesystem.tex
mv typesystem.pdf ../pdfs
#
pdflatex -shell-escape ../src/tex/linkagearch.tex
pdflatex -shell-escape ../src/tex/linkagearch.tex
mv linkagearch.pdf ../pdfs
#
pdflatex -shell-escape ../src/tex/felix-ref.tex
pdflatex -shell-escape ../src/tex/felix-ref.tex
makeindex felix-ref
makeglossaries felix-ref
pdflatex -shell-escape ../src/tex/felix-ref.tex
mv felix-ref.pdf ../pdfs
#
pdflatex -shell-escape ../src/tex/technote-compact-linear-types.tex
pdflatex -shell-escape ../src/tex/technote-compact-linear-types.tex
mv technote-compact-linear-types.pdf ../pdfs
#
pdflatex -shell-escape ../src/tex/technote-row-polymorphism.tex
pdflatex -shell-escape ../src/tex/technote-row-polymorphism.tex
mv technote-row-polymorphism.pdf ../pdfs
#
pdflatex -shell-escape ../src/tex/ci.tex
pdflatex -shell-escape ../src/tex/ci.tex
mv ci.pdf ../pdfs

pdflatex -shell-escape ../src/tex/coroutines.tex
pdflatex -shell-escape ../src/tex/coroutines.tex
mv coroutines.pdf ../pdfs
#
pdflatex -shell-escape ../src/tex/modern_programming.tex
pdflatex -shell-escape ../src/tex/modern_programming.tex
makeindex modern_programming
makeglossaries modern_programming
pdflatex -shell-escape ../src/tex/modern_programming.tex
mv modern_programming.pdf ../pdfs

#
#pdflatex -shell-escape ../src/tex/coprogramming.tex
#pdflatex -shell-escape ../src/tex/coprogramming.tex
#makeindex coprogramming
#makeglossaries coprogramming
#pdflatex -shell-escape ../src/tex/coprogramming.tex
#mv coprogramming.pdf ../pdfs
cd ..
sh parsedoc.sh

