
#' ---
#' title: "Exploratory Analysis"
#' output: html_document
#' ---

#+ setup, echo=FALSE, results='hide', warning=FALSE, message=FALSE

library(tidyverse)
library(skimr)
library(reticulate)
cluster <- import("sklearn.cluster")
library(magrittr)
library(ggforce)
library(infotheo)
library(corrplot)

knitr::opts_chunk$set(echo=FALSE, results='hide')

#' ### Load

#' how to load data?  
#' we maybe don't want to use prepped.data...  
#' unless we wanna bloat it to include a copy of the original data
#' or if we're okay doing this with some cleaning already done  
#' ya, we're okay with that  
#' so 'loans' in prepped.data hass...  
#' - main predictors renamed  
#' 

#+ load

load(file="prepped.data")

#' ### Re-skim
#+ skim, results='show', warning=FALSE

skim(all.loans)

#' ### Questions

#' - What variance do we see in our outcome?
#+ var, results='show'

# geom_bar uses counts, _col values
plt <- ggplot(all.loans, aes(action_taken_name)) + geom_bar() + theme(axis.text.x = element_text(angle=45, hjust=1))
print(plt)

#'   - How does this variance map onto...  
#'     - income  
#'     - race  
#'     - property type
#' - What does the data look like in dim-reduced predictor space?  

#+
#' ### Peek at relationships
#+ pairgrid, results='show'

grid <- loans %>% drop_na() %>%
                    ggplot(aes(x=.panel_x, y=.panel_y)) + 
                    geom_point(shape=16, size=.5, position="auto") + 
                    geom_autodensity() +
                    geom_density2d() +
                    facet_matrix(vars(c(ind.cont.pred, loc.cont.pred)),
                                layer.diag=2, layer.upper=3)

print(grid)




#' Categorical variables wanna have fun too
#+ mutinfo, echo=TRUE, results='show'

mi.loans <- discretize(loans)
mi <- mutinformation(mi.loans[c(ind.cat.pred, ind.cont.pred, "action_taken")])
print(mi)

print(corrplot(mi, is.corr=FALSE, method="color"))


