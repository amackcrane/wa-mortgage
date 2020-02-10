


svm: svm.R prepped.data
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('svm.R', knit=FALSE), output_dir='visualization')"

svm_refit: svm.R prepped.data
	rm temp.svcv
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('svm.R', knit=FALSE), output_dir='visualization')"

# visualize using saved models, if they exist
randomforest: randomforest.R prepped.data
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('randomforest.R', knit=FALSE), output_dir='visualization')"

# delete saved models to force refitting
randomforest_refit: randomforest.R prepped.data
	rm -f temp.rfcv
	rm -f temp.all.rfcv
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('randomforest.R', knit=FALSE), output_dir='visualization')"


cluster: prepped.data cluster.R
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('cluster.R', knit=FALSE), output_dir='visualization')"

eda: eda.R prepped.data
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('eda.R', knit=FALSE), output_dir='visualization')"


prepped.data: prep.R
	Rscript prep.R

visualization/prep.html: prep.R
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('prep.R', knit=FALSE), output_dir='visualization')"
