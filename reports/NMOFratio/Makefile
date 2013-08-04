pdf: 
	R CMD Sweave NMOFratio.Rnw
	latexmk -pdfps NMOFratio.tex
www: NMOFratio.tex
	R CMD Stangle NMOFratio.Rnw
	cp NMOFratio.pdf /home/es/ESdotnet/files/NMOFratio.pdf
	cp NMOFratio.R /home/es/Packages/NMOF/inst/NMOFex/NMOFratio.R
clean: 
	latexmk -C NMOFratio.tex