pdf: 
	R CMD Sweave NMOFtests.Rnw
	latexmk -pdfps NMOFtests.tex
www: NMOFtests.tex
	R CMD Stangle NMOFtests.Rnw
	cp NMOFtests.pdf /home/es/ESdotnet/files/NMOFtests.pdf
	cp NMOFtests.R /home/es/Packages/NMOF/inst/NMOFex/NMOFtests.R
clean: 
	latexmk -C NMOFtests.tex
