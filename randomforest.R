
#' ---
#' title: "WA Loan analysis w/ Random Forest"
#' output: html_document
#' ---

#+ setup, echo=FALSE, results='hide', warning=FALSE, message=FALSE
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


rf <- ensemble$RandomForestClassifier(oob_score=TRUE, n_estimators=10L)

# we'll cross-validate over max_features (to consider at each split)
#pg <- list(max_features=range(1, 20))
pg <- seq(1L, 2L)

# seq!!!!!! not range!


# use RFC.oob_score_! as generalization estimator
#oob.score <- metrics$make_scorer(function(est){return(est$oob_score_)}, greater_is_better=TRUE)
# nah. doesn't play nice w/ build in CV facilites


X <- loans.fixed
y <- outcome.fixed

#' ### Fit test
#+ test.fit, results='show', echo=TRUE

test.fit <- rf$fit(X, y)
print(test.fit$oob_score_)

#save(rf, test.fit, file="temp.test.fit")
py_save_object(test.fit, "temp.test.fit")


#' ### Fit
#+ fit, results='show', echo=TRUE

rfcv <- selection$validation_curve(estimator=rf, X=X, y=y, param_name="max_features", param_range=pg, cv=2L, scoring='accuracy')

#save(rfcv, file="temp.rfcv")
py_save_object(rfcv, "temp.rfcv")

#' ### Viz
#+ viz, results='show'

#plt <- ggplot(aes(pg, rfcv$train_scores, rfcv$test_scores)) + geom_line()
#print(plt)

save.image(file="temp.models")



