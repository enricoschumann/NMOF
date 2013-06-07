pdf: 
	R CMD Sweave NMOFdist.Rnw
	latexmk -pdfps NMOFdist.tex
www: NMOFdist.tex
	R CMD Stangle NMOFdist.Rnw
	cp NMOFdist.pdf /home/es/ESdotnet/files/NMOFdist.pdf
	cp NMOFdist.R /home/es/Packages/NMOF/inst/NMOFex/NMOFdist.R
clean: 
	latexmk -C NMOFdist.tex