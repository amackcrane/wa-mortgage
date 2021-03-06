

#' ---
#' title: "SVM w/ Loan Data"
#' output: html_document
#' ---


#+ setup, echo=FALSE, results='hide', warning=FALSE, message=FALSE

start <- Sys.time()

library(tidyverse)
library(reticulate)
svm <- import("sklearn.svm")
selection <- import("sklearn.model_selection")
metrics <- import("sklearn.metrics")
knitr::opts_chunk$set(echo=FALSE, results='hide')


#+ load

load(file='prepped.data')


#' ### Spec model
#+ spec, echo=TRUE, results='show'

svc <- svm$LinearSVC(loss='squared_hinge', dual=FALSE)

X <- loans.fixed
y <- outcome.fixed

#' ### Quick test
#+ test, echo=FALSE, results='show', eval=FALSE
'
svc <- svc$set_params(C=1.0)

# speedy-like
inds <- sample.int(nrow(X), 3000, replace=FALSE)
testX <- X[inds,]
testy <- y[inds]

svc <- svc$fit(testX, testy)
py_save_object(svc, file="temp.svc")
print(svc)
print(svc$score(testX, testy))
'
#' ### CV
#+ cv, echo=TRUE, results='show', eval=TRUE

pg <- list()
pg$C <- c(.0001, .001, .01, .1, 1)

svcv <- selection$GridSearchCV(estimator=svc, param_grid=pg, scoring='accuracy', cv=5L)

svcv <- tryCatch({

    py_load_object(file="temp.svcv")
}, error=function(e){

    svcv <- svcv$fit(X, y)
    py_save_object(svcv, file="temp.svcv")
    return(svcv)
})

#' ### Viz
#+ viz, echo=FALSE, results='show'

cv.results <- data.frame(svcv$cv_results_)
cv.results$param_C <- unlist(cv.results$param_C)

plt <- ggplot(cv.results, aes(param_C, mean_test_score)) + geom_line()
print(plt)

end <- Sys.time()
time <- end - start
print(time)

#' ### Timing
#' one model, 3000 obs -- 30s  
#' pg=4, cv=5, full set -- 33m  
#' pg=5, cv=5, full set -- 29m (C range was lower...)  
#' pg=4, cv=5, SVC (LIBSVC) -- 10 hrs & still chuggin  



