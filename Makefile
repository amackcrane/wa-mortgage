

#visualization/randomforest.html: randomforest.R prepped.data
#	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('randomforest.R', knit=FALSE), output_dir='visualization')"

# visualize using saved models, if they exist
randomforest: randomforest.R prepped.data
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('randomforest.R', knit=FALSE), output_dir='visualization')"

# delete saved models to force refitting
randomforest_refit: randomforest.R prepped.data
	rm temp.rfcv
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('randomforest.R', knit=FALSE), output_dir='visualization')"



# this is weird and maybe not great?
prepped.data: prep.R
	Rscript prep.R

visualization/prep.html: prep.R
	Rscript -e "library(rmarkdown); rmarkdown::render(knitr::spin('prep.R', knit=FALSE), output_dir='visualization')"
