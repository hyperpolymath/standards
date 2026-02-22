# SPDX-License-Identifier: PMPL-1.0-or-later
.PHONY: all clean pdf

all: pdf

pdf:
	pdflatex a2ml-arxiv.tex
	pdflatex a2ml-arxiv.tex  # Second pass for references
	@echo "PDF generated: a2ml-arxiv.pdf"

clean:
	rm -f *.aux *.log *.out *.pdf *.bbl *.blg

view: pdf
	xdg-open a2ml-arxiv.pdf 2>/dev/null || open a2ml-arxiv.pdf
