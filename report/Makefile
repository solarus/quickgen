pdf:
	emacs -q --batch --eval "(setq org-latex-to-pdf-process '(\"latexmk -f -latexoption=-shell-escape -pdf %f\"))" --visit report.org --funcall org-export-as-pdf

clean:
	rm -f *.{aux,bbl,bcf,blg,dvi,fdb_latexmk,fls,log,out,pdf,pyg,run.xml,tex,tex.blg,toc}
	rm -f report.tex~
	rm -f logo_gu-eps-converted-to.pdf
