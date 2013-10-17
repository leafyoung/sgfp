
pdf1:
	pandoc talk.md --slide-level 2 -t beamer -o talk.tex -V theme:Montpellier
	pdflatex sgfp20131017-mutables.tex
	open sgfp20131017-mutables.pdf

tex:
	pandoc --slide-level 2 -t beamer talk.md -o talk.tex -V theme:Warsaw

pdfdirect:
	pandoc --slide-level 2 -t beamer talk.md -o talk.pdf -V theme:Singapore
	open talk.pdf

