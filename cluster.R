
#' ---
#' title: "Clustering"
#' output: html_document
#' ---

#+ setup, echo=FALSE, warning=FALSE, message=FALSE, results='hide'

start <- Sys.time()

library(tidyverse)
library(reticulate)
library(magrittr)
library(cluster)
library(stats)
library(ggfortify)
library(corrplot)

#pp <- import("sklearn.preprocessing", convert=TRUE)

knitr::opts_chunk$set(echo=FALSE, results='hide')

# Load
load(file="prepped.data")

# Clean up
keep <- c("loans.fixed")
rm(list=setdiff(ls(), keep))


#' ### Compute Clustering
#+ cluster, results='show', echo=TRUE

small.data <- loans.fixed[sample(nrow(loans.fixed), 2000),] %>% data.frame

# Clean up
rm(loans.fixed)

# toss any columns with no variance d/t sampling & low-frequency categories
print(dim(small.data))
small.data %<>% select_if(~ var(.) > 0)
print(dim(small.data))

clust <- pam(small.data, k=4, metric="euclidean")


#' ### Compute PCA
#+ pca, results='show', echo=TRUE

pca <- prcomp(small.data, tol=.3)

# Get loadings
# grab eigenvectors (as columns here)
egvec <- pca$rotation
#print(head(egvec))

# multiply through by eigenvalues...
egval <- pca$sdev * pca$sdev
#print(egval)
egval <- egval[1:ncol(egvec)]
load <- apply(egvec, 1, function(x) x * egval) %>% t
load %<>% set_rownames(colnames(small.data))
#print(head(load))

# sort (hackishly)
x.scores <- apply(load, 1, compose(sum, abs))
#print(x.scores)
sort.order <- order(x.scores, decreasing=TRUE)
abs.load <- load[sort.order,] %>% abs

# truncate
abs.load <- abs.load[1:10,]

top <- max(abs.load)

# TODO map varnames onto X's
# varnames are...
#rownames(abs.load) <- colnames(loans.fixed)[sort.order][1:10]

# Loadings heatmap
corrplot(abs.load, is.corr=FALSE, cl.lim=c(0,top), method="color")



#' ### Clustering Plot
#+ plot, results='show', echo=TRUE

# Gotta DIY it afaict if we want a limited set of loadings
# b/c I don't think we can control what ggfortify sends as
#   loadings.data to ggbiplot

# pass data points in PC-space
pcs <- c(1:2)
pc.data <- pca$x[,pcs] %>% data.frame
# grab clusters
pc.data$cluster <- clust$clustering
# fix loadings
load.dat <- load[1:4,pcs] %>% data.frame

ggbiplot(pc.data, loadings.data=load.dat, loadings.label=TRUE, colour='cluster')

#autoplot(clust, loadings=TRUE, loadings.data=abs.load[1:3], loadings.label=TRUE,
#         x=1, y=2)

