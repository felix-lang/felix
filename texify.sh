cd workspace 
pdflatex -shell-escape ../src/tex/$1.tex
pdflatex -shell-escape ../src/tex/$1.tex
mv $1.pdf ../pdfs

