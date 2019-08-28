prover: 
	ghc -Wall Main

report:
	pdflatex Report.tex


clean:
	rm -f *.hi *.o *.aux *.log *.pdf
	rm -f Main
