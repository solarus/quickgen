pdf:
	emacs -q --batch \
	  --eval "(setq package-load-list '((org t)))" \
	  --eval "(package-initialize)" \
	  --eval "(setq org-latex-classes '((\"myreport\" \"\\\documentclass[11pt]{report}\" (\"\\\chapter{%s}\" . \"\\\chapter*{%s}\") (\"\\\section{%s}\" . \"\\\section*{%s}\") (\"\\\subsection{%s}\" . \"\\\subsection*{%s}\") (\"\\\subsubsection{%s}\" . \"\\\subsubsection*{%s}\") (\"\\\paragraph{%s}\" . \"\\\paragraph*{%s}\"))))" \
	  --eval "(setq org-latex-pdf-process '(\"pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f\" \"biber %b.bcf\" \"pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f\"))" \
	  --visit report.org --funcall org-latex-export-to-pdf

org:
	emacs -q --batch \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.milkbox.net/packages/\") t)" \
	  --eval "(package-install 'org)"
clean:
	rm -f *.{aux,bbl,bcf,blg,dvi,fdb_latexmk,fls,log,out,pdf,pyg,run.xml,tex,tex.blg,toc}
	rm -f report.tex~
	rm -f logo_gu-eps-converted-to.pdf
