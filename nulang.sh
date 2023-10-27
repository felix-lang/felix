cd workspace
pdflatex -shell-escape ../src/tex/nulang.tex
biber nulang 
pdflatex -shell-escape ../src/tex/nulang.tex
cp nulang.pdf ../pdfs
