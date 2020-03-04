
#' ---
#' title: "First Look"
#' output: html_document
#' ---

#+ setup, echo=FALSE, warning=FALSE, message=FALSE, results='hide'

start <- Sys.time()

library(tidyverse)
library(skimr)
library(reticulate)
library(magrittr)

pp <- import("sklearn.preprocessing", convert=TRUE)

knitr::opts_chunk$set(echo=FALSE, results='hide')

#+ load

loans <- read.csv(file="Washington_State_HDMA-2016.csv", 
                 header=TRUE, sep=",")


#' first drop an outlier that's messing up histograms...
#' and drop respondent id
#+ echo=TRUE

loans %<>% filter(loan_amount_000s < 10000)


#' Peek at variables
#' And drop useless ones
#+ skim, echo=TRUE, results='show', warning=FALSE
skim_without_charts(loans)

loans %<>% select(-respondent_id, -state_name, -state_abbr)


# save a copy for later
all.loans <- loans



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


#+

#' ### Drop NAs for selected vars
#' Also drop obs with too-rare outcome value
#+ dropna, echo=TRUE, results='show'

print(nrow(loans))
loans %<>% drop_na(c(cat, cont, "action_taken"))
print(nrow(loans))

# We need outcome categories to have more members than the folds in our CV
loans %<>% add_count(action_taken) %>% filter(n > 4) %>% select(-n)

#' ### Standardize continuous vars  

#+ scaling, results='show'


#pp$StandardScaler
# fit, transform, inverse_transform

scaler <- pp$StandardScaler()
loans.fixed <- scaler$fit_transform(loans[cont])
colnames(loans.fixed) <- colnames(loans[cont])

#' ### Transform categorical vars

#+ onehot, results='show'

onehot <- pp$OneHotEncoder(drop='first', sparse=FALSE)
cat.fixed <- onehot$fit_transform(loans[cat])
colnames(cat.fixed) <- onehot$get_feature_names(colnames(loans[cat]))

loans.fixed <- cbind(loans.fixed, cat.fixed)

# We need outcome var transformed too
label <- pp$LabelEncoder()
outcome.fixed <- label$fit_transform(loans$action_taken)


# more readable
print(as_tibble(loans.fixed))
print(as_tibble(outcome.fixed))



#' ### All data variant  
#' Count NAs cond. on variable inclusion!  

#+ all.data.drop, echo=TRUE, results='show', eval=TRUE

count.na <- function(df){
    # good idea but nah
    # df %>% rowwise %>% summarize(any.na = any(is.na(colnames(.)))) %>% pull(any.na) %>% sum
    df %>% pmap(~ any(is.na(c(...)))) %>% as.logical %>% sum
}

# Count union of NA obs across subsets of variables
test.dropna <- function(df){
    # get list mapping feature names to na count
    na.count <- sapply(df, function(col) sum(is.na(col)))
    # sort
    na.count <- na.count[order(na.count, decreasing=FALSE)]
    # create new df
    test.df <- NULL
    union.na.count <- list()
    # add features one by one
    for(f in names(na.count)){
        if(is.null(test.df)) test.df <- data.frame(df[f])
        else test.df[f] <- df[f]

        # check union NA count
        union.na.count[f] <- count.na(test.df)
    }

    union.na.count$obs <- nrow(df)
    return(union.na.count)
}

#all.loans %>% sample_frac(.01) %>% test.dropna %>% tail
# rate_spread is the only real bad one
all.loans %<>% select(-rate_spread)

# drop NAs
all.loans %<>% drop_na()

#' All data transform
#+ all.data.transform, results='show', echo=TRUE

index.numeric <- function(df){
    sapply(colnames(df), function(name) is.numeric(df[,name]))
}
index.factor <- function(df){
    sapply(colnames(df), function(name) is.factor(df[,name]))
}

# first, break out the outcome, we gotta treat that differently
all.outcome <- all.loans$action_taken_name
all.loans$action_taken_name <- NULL
all.loans %<>% select(-matches("denial_reason"))

# do we lose any columns by assuming non-numeric ==> factor?
print(ncol(all.loans))
num <- index.numeric(all.loans)
fac <- index.factor(all.loans)
print(length(c(num[num], fac[fac])))

# setup scalers
all.scaler <- pp$StandardScaler()
all.onehot <- pp$OneHotEncoder(sparse=FALSE)

# fit, transform, keep colnames
all.cont <- all.scaler$fit_transform(all.loans[,num])
colnames(all.cont) <- colnames(all.loans[,num])

all.cat <- all.onehot$fit_transform(all.loans[,fac])
colnames(all.cat) <- all.onehot$get_feature_names(colnames(all.loans[,fac]))

# transform outcome
all.label <- pp$LabelEncoder()
all.outcome.fixed <- all.label$fit_transform(all.outcome)

# DOES NOT WORK when all.cat is sparse!!!!!!!!!!!!!!!!!!!!
all.loans.fixed <- cbind(all.cont, all.cat)


#+ finish, echo=FALSE, results='show'

# Time
end <- Sys.time()
print(end - start)


# Save
keep <- c("loans.fixed", "outcome.fixed", "all.loans.fixed", "all.outcome.fixed",
          "scaler", "onehot", "label", "all.scaler", "all.onehot", "all.label")
rm(list=setdiff(ls(), keep))
save.image(file='prepped.data')


#' ### Timing
#' 47 features, 450000 obs -- 1.4m  
#' 2.5m as Rscript  
#' 7.5m rmd w/ test.dropna  
#' 3.5m rmd w test.dropna on 1% of data   
#' 2.2m rmd w/o test.dropna  
#' 12m w/ all.fixed non-sparse  




