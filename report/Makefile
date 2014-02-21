pdf:
	emacs -q --batch --eval "(setq org-latex-to-pdf-process '(\"latexmk -f -pdf %f\"))" --visit report.org --funcall org-export-as-pdf

clean:
	rm -f proposal.{bbl,bcf,blg,dvi,fdb_latexmk,fls,pdf,run.xml,tex,tex.blg}
	rm -f logo_gu-eps-converted-to.pdf
