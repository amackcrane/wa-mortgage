

site: svm.Rmd randmoforest.Rmd cluster.Rmd eda.Rmd prepped.data
	Rscript -e "library(rmarkdown); rmarkdown::run(file='index.Rmd', shiny_args=list(launch.browser=TRUE))"



svm_refit:
	rm -f temp.svcv
	rm -f temp.all.rfcv
	make svm

svm: visualization/svm.pdf

visualization/svm.pdf: svm.Rmd prepped.data
	Rscript -e "library(rmarkdown); rmarkdown::render('svm.Rmd', output_dir='visualization', output_format='pdf_document')"

svm.Rmd: svm.R
	Rscript -e "library(rmarkdown; knitr::spin('svm.R', knit=FALSE)"


# visualize using saved models, if they exist
# delete saved models to force refitting
randomforest_refit: 
	rm -f temp.rfcv
	rm -f temp.all.rfcv
	make randomforest

randomforest: visualization/randomforest.pdf

visualization/randomforest.pdf: randomforest.Rmd prepped.data
	Rscript -e "library(rmarkdown); rmarkdown::render('randomforest.Rmd', output_dir='visualization', output_format='pdf_document')"

randomforest.Rmd: randomforest.R
	Rscript -e "library(rmarkdown); knitr::spin('randomforest.R', knit=FALSE)"

cluster: visualization/cluster.pdf

visualization/cluster.pdf: prepped.data cluster.R
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('cluster.R', knit=FALSE), output_dir='visualization', output_format='pdf_document')"

eda: visualization/eda.pdf

visualization/eda.pdf: eda.R prepped.data
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('eda.R', knit=FALSE), output_dir='visualization', output_format='pdf_document')"


prepped.data: prep.R
	Rscript prep.R

visualization/prep.pdf: prep.R
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('prep.R', knit=FALSE), output_dir='visualization', output_format='pdf_document')"
