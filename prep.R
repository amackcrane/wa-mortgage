
#' ---
#' title: "First Look"
#' output: html_document
#' ---

#+ setup, echo=FALSE, warning=FALSE, message=FALSE, results='hide'

library(tidyverse)
# gonna try import()ing python modules lol
library(reticulate)
library(magrittr)
library(ggforce)
library(corrplot)
library(infotheo)

knitr::opts_chunk$set(echo=FALSE, results='hide')

#+ load

loans <- read.csv(file="Washington_State_HDMA-2016.csv", 
                 header=TRUE, sep=",")


#' first drop an outlier that's messing up histograms...
#+ echo=TRUE

loans %<>% filter(loan_amount_000s < 10000)




#' let's recode some NAs  
#' Actually let's not! just throwing away info  
#+ factor_clean, eval=FALSE

'
loans %<>% mutate(ethnicity = fct_recode(applicant_ethnicity_name,
                                         "NA" = "Information not provided by applicant in mail, Internet, or telephone application",
                                         "NA" = "Not applicable"),
                  race = fct_recode(applicant_race_name_1,
                                    "NA" = "Information not provided by applicant in mail, Internet, or telephone application",
                                    "NA" = "Not applicable"))

summary(loans[c("ethnicity", "race")])
'

#' Fix some other varnames
#+ names, echo=TRUE
loans %<>% rename(ethnicity=applicant_ethnicity_name, race=applicant_race_name_1,
                  sex = applicant_sex_name, county = county_name, msamd = msamd_name,
                  purchaser_type = purchaser_type_name, property_type = property_type_name,
                  loan_purpose = loan_purpose_name, preapproval = preapproval_name,
                  hoepa_status = hoepa_status_name, loan_type = loan_type_name,
                  owner_occupancy = owner_occupancy_name, lien_status = lien_status_name,
                  action_taken = action_taken_name)


#' Categorize our variables

#+ echo=TRUE


ind.cont.pred <- c("loan_amount_000s", "applicant_income_000s")
loc.cont.pred <- c("hud_median_family_income", "tract_to_msamd_income", "population",
                   "minority_population", "number_of_owner_occupied_units",
                   "number_of_1_to_4_family_units")


ind.cat.pred <- c("ethnicity", "race", "sex")
loc.cat.pred <- c("county", "msamd")
trans.cat.pred <- c("purchaser_type", "property_type", "loan_purpose", 
                    "preapproval", "hoepa_status", "loan_type",
                    "owner_occupancy", "lien_status")

cont <- c(ind.cont.pred, loc.cont.pred)

cat <- c(ind.cat.pred, loc.cat.pred, trans.cat.pred)

#' outcome: action_taken_name  

#' look at variables
#+ echo=TRUE, results='show'
summary(loans[ind.cat.pred])
summary(loans[ind.cont.pred])
summary(loans[loc.cat.pred])
summary(loans[trans.cat.pred])
summary(loans[loc.cont.pred])
glimpse(loans)


#' ### Peek at relationships
#+ pairgrid, results='show', eval=FALSE

'
grid <- loans %>% drop_na() %>%
                    ggplot(aes(x=.panel_x, y=.panel_y)) + 
                    geom_point(shape=16, size=.5, position="auto") + 
                    geom_autodensity() +
                    geom_density2d() +
                    facet_matrix(vars(c(ind.cont.pred, loc.cont.pred)),
                                layer.diag=2, layer.upper=3)
'


# layer.diag=2
# hm catching some errors with facet_matrix. maybe doesn't like categorical vars?
#  is working w/o cat vars
# complained about NAs, which, duh
#print(grid)



#' Categorical variables wanna have fun too
#+ mutinfo, echo=TRUE, results='show'

mi.loans <- discretize(loans)
mi <- mutinformation(mi.loans[c(ind.cat.pred, ind.cont.pred, "action_taken")])
print(mi)

print(corrplot(mi, is.corr=FALSE, method='color'))



#' ### Some questions!!!  
#' - how do you do feature selection?  
#'   - do some methods (L2 reg?) not care about extra features?  
#' - is there some middle ground between looking at bivariate relationships and fitting the whole model and seeing what sticks?  
#' - how to work with mutual information?  
#'   - is it a reasonable approach for inspecting dyadic relationships w/ categorical vars?  
#'   - can it tell you something about monotonicity?  
#'   - why is a variable's mutual information with itself not 1?  
#'     - is auto-MI just entropy??  
#'   - is mutual information sensitive to binning?  
#' - are PCA or other unsupervised methods relevant to predictive work?  


#+

#' ### Standardize continuous vars  
#' using reticulate?

#+ scaling, results='show'

pp <- import("sklearn.preprocessing", convert=TRUE)

#pp$StandardScaler
# fit, transform, inverse_transform

# kinda gross but ...
scalers <- map(rep(TRUE, length(cont)), pp$StandardScaler)
# give names to list
names(scalers) <- cont

loans.fixed <- vector("list", length(cont))

for(i in seq(length(cont))){
    name <- cont[i]
    loans.fixed[[i]] <- scalers[[name]]$fit_transform(loans[name])
}

names(loans.fixed) <- cont
loans.fixed <- as.data.frame(loans.fixed)

#s.cont <- map2(rep("s.", length(cont)), cont, partial(paste, sep=""))


#' ### Transform categorical vars to one-hot

#+ onehot, results='show'






head(loans.fixed)

# getting imported objects to instantiate...
#  no parens (cuz wants function obj, duh) + convert=FALSE
#   seems to work
#  no parens + convert=TRUE
#   also works!
#  $new() + convert=TRUE


# Keep some ancillary vars around for analysis
save.image(file='prepped.data')



