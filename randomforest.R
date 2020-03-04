
#' ---
#' title: "WA Loan analysis w/ Random Forest"
#' output: html_document
#' ---

#+ setup, echo=FALSE, results='hide', warning=FALSE, message=FALSE


# load as workspace image s.t. we don't have to hardcode lists of relevant vars...
load(file='prepped.data')
keep <- c("loans.fixed", "outcome.fixed", "all.loans.fixed", "all.outcome.fixed")
rm(list=setdiff(ls(), keep))


start <- Sys.time()

library(tidyverse)
library(magrittr)
library(reticulate)
ensemble <- import("sklearn.ensemble")
selection <- import("sklearn.model_selection")
metrics <- import("sklearn.metrics")
knitr::opts_chunk$set(echo=FALSE, results='hide')




#' # Model Setup

#+ prep, results='show', echo=TRUE

print(ls())

rf <- ensemble$RandomForestClassifier(oob_score=TRUE, n_estimators=100L)

# we'll cross-validate over max_features (to consider at each split)
#pg <- list(max_features=range(1, 20))
pg <- seq(.1, .9, .1)

# seq!!!!!! not range!
samples <- sample(nrow(loans.fixed), 10000)

X <- loans.fixed[samples,] %>% set_colnames(colnames(loans.fixed))
y <- outcome.fixed[samples]

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
    # Hold OOB scores & feature importances
    oob_scores <- vector(len=length(pg))
    feature_imp <- matrix(nrow=length(pg), ncol=ncol(X)) %>%
                   set_colnames(colnames(X))

    # Loop over max_features
    for(i in seq_along(pg)){
	rf$set_params(max_features=pg[i])
	fit <- rf$fit(X, y)
	oob_scores[i] <- fit$oob_score_

        feature_imp[i,] <- fit$feature_importances_
    }

    # pop in dataframe
    rfcv <- data.frame(oob=oob_scores, max_features=pg)
    rfcv <- cbind(rfcv, feature_imp)

    py_save_object(rfcv, "temp.rfcv")
    return(rfcv)
})
 
#+ all_fit, restuls='show', echo=TRUE

samples <- sample(nrow(all.loans.fixed), 10000)
X <- all.loans.fixed[samples,] %>% set_colnames(colnames(all.loans.fixed))
y <- all.outcome.fixed[samples]

# refitting governed by Makefile:
all.rfcv <- tryCatch({

    # if fitted model exists, load it
    py_load_object(file="temp.all.rfcv")
}, error=function(e){

    # if not...
    # Hold OOB scores
    oob_scores <- vector(len=length(pg))
    feature_imp <- matrix(nrow=length(pg), ncol=ncol(X)) %>%
                   set_colnames(colnames(X))
    
    # Loop over max_features
    for(i in seq_along(pg)){
	rf$set_params(max_features=pg[i])
	fit <- rf$fit(X, y)
	oob_scores[i] <- fit$oob_score_
        feature_imp[i,] <- fit$feature_importances_
    }

    # pop in dataframe
    all.rfcv <- data.frame(oob=oob_scores, max_features=pg)
    all.rfcv <- cbind(all.rfcv, feature_imp)

    py_save_object(all.rfcv, "temp.all.rfcv")
    return(all.rfcv)
})
 


#' ### Visualize Cross-Validation
#+ cv_viz, results='show'

# Specify
plt_fun <- function(x) x %>% ggplot(aes(max_features, oob)) + geom_line() +
                   ylab("Test score") + xlab("# Features considered at each node")

# Exec
rfcv %>% plt_fun %>% + ggtitle("Hand-Selected Features") %>% print

all.rfcv %>% plt_fun %>% + ggtitle("All Features") %>% print


#' ### Feature Importances
#+ importances, echo=TRUE, results='show'
'
imp <- rfcv$feature_importances_ %>% set_names(colnames(loans.fixed)) %>%
                 data.frame(importance=.) %>%
                 rownames_to_column("feature")
all.imp <- all.rfcv$feature$importances_ %>% set_names(colnames(all.loans.fixed)) %>%
                 data.frame(importance=.) %>%
                 rownames_to_column("feature")
'

# Specify

# wrap strings lacking whitespace e.g. variable names
strwrap_force <- function(col, width=20) {
    purrr::map_chr(strsplit(col, ""),
                   ~ purrr::reduce2(.x,
                                    seq_along(.x),
                                    ~ paste(..1, rep("\n", mod(..3, width) == 0), ..2, sep=""),
                                    .init=""))
}

# colnames already set...
# summarize, transpose, set rownames to variable
get_importances <- function(x) x %>% select(-oob, -max_features) %>% lapply(mean) %>%
    data.frame %>%
    set_rownames(c("importance")) %>%
    t %>% data.frame %>% rownames_to_column("feature") %>%
    mutate(feature=strwrap_force(feature, 20))


# let's sort and truncate
sort_trunc <- function(x) x[order(x$importance, decreasing=TRUE),][1:15,]

# plot template
plt_fun <- function(x) x %>% get_importances %>% sort_trunc %>%
    ggplot() %>%
    + geom_col(aes(feature, importance)) %>%
#    + theme(axis.text.x = element_text(angle=45, hjust=1)) %>%
    + coord_flip()

# Exec
rfcv %>% plt_fun %>%
    + ggtitle("Hand-Sel.") %>%
    print
all.rfcv %>% plt_fun %>%
    + ggtitle("All Feat.") %>%
    print


#+ end, results='show'
end <- Sys.time()
time <- end - start
print(time)


#' ### Time Notes
#' trees, params, time  
#' 10, 2, 1.5m (was this Rscript or rmd?)  
#' 100, 9, 47m  
#' pre-fitted, 1m (rmd)  
#' 100, 19, [dunno cause error] (rmd)  
