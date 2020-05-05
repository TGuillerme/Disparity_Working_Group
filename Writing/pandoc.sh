sed 's/textcolor{blue}{/textcolor{blue}{COLORTAGIN[/g' disparity_review.tex > tmp
sed 's/}/]COLORTAGOUT}/g' tmp > compile.tex
pandoc -o disparity_review.docx -i compile.tex --bibliography References.bib --filter pandoc-citeproc --csl biologyletters.csl
rm compile.tex
rm tmp