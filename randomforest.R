
#' ---
#' title: "WA Loan analysis w/ Random Forest"
#' output: html_document
#' ---

#+ setup, echo=FALSE, results='hide', warning=FALSE, message=FALSE

start <- Sys.time()

library(tidyverse)
library(reticulate)
ensemble <- import("sklearn.ensemble")
selection <- import("sklearn.model_selection")
metrics <- import("sklearn.metrics")
knitr::opts_chunk$set(echo=FALSE, results='hide')


#+ load
# load as workspace image s.t. we don't have to hardcode lists of relevant vars...
load(file='prepped.data')



#' ### Model Setup

#+ prep, results='show', echo=TRUE


rf <- ensemble$RandomForestClassifier(oob_score=TRUE, n_estimators=100L)

# we'll cross-validate over max_features (to consider at each split)
#pg <- list(max_features=range(1, 20))
pg <- seq(2L, 20L)

# seq!!!!!! not range!


X <- loans.fixed
y <- outcome.fixed

#' ### Fit test
#+ test.fit, eval=FALSE
'
test.fit <- rf$fit(X, y)
print(test.fit$oob_score_)

py_save_object(test.fit, "temp.test.fit")
'

#' ### Fit
#' Evaluate using oob_score_  


#+ fit, results='show', echo=TRUE

# refitting governed by Makefile:
rfcv <- tryCatch({

    # if fitted model exists, load it
    py_load_object(file="temp.rfcv")
}, error=function(e){

    # if not...
    # Hold OOB scores
    oob_scores <- vector(len=length(pg))

    # Loop over max_features
    for(i in seq_along(pg)){
	rf$set_params(max_features=pg[i])
	fit <- rf$fit(X, y)
	oob_scores[i] <- fit$oob_score_
    }

    # pop in dataframe
    rfcv <- data.frame(oob=oob_scores, max_features=pg)

    py_save_object(rfcv, "temp.rfcv")
    return(rfcv)
})



#' ### Viz
#+ viz, results='show'

plt <- ggplot(rfcv, aes(max_features, oob)) + geom_line() #+ ylim(0, 1)
print(plt)

end <- Sys.time()
time <- end - start
print(time)


#' ### Time Notes
#' trees, params, time  
#' 10, 2, 1.5m (was this Rscript or rmd?)  
#' 100, 9, 47m  
#' pre-fitted, 1m (rmd)  
#' 100, 19, [dunno cause error] (rmd)  
