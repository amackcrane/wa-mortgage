
#' ---
#' title: "Clustering"
#' output: html_document
#' ---

#+ setup, echo=FALSE, warning=FALSE, message=FALSE, results='hide'

# Load
load(file="prepped.data")

# Clean up
keep <- c("loans.fixed", "outcome.fixed")
rm(list=setdiff(ls(), keep))

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



#' # Compute Clustering
#+ cluster, results='show', echo=TRUE

sample.ind <- sample(nrow(loans.fixed), 2000)
small.data <- loans.fixed[sample.ind,] %>% data.frame
small.outcome <- outcome.fixed[sample.ind]

# Clean up
rm(loans.fixed, outcome.fixed)

# toss any columns with no variance d/t sampling & low-frequency categories
print(dim(small.data))
small.data %<>% select_if(~ var(.) > 0)
print(dim(small.data))

clust <- pam(small.data, k=4, metric="euclidean")


#' # Compute PCA
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



#' # Clustering Plot
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
# pull in names
load.dat$var <- rownames(load.dat)

#ggbiplot(pc.data, loadings.data=load.dat, loadings.label=TRUE, colour='cluster')



## Add in outcome var??
pc.data$outcome <- small.outcome
# take 2 data.frames
#   one the data points mapped into PC space w/ cols named PCX; cluster
#     created by e.g. prcomp + cluster::pam
#   the other variable loadings into PC space, cols named PCX; var
#     which should reflect eigenvalue magnitudes
# color_varname pertains to the 'pc' data.frame
pca.loadings.plot <- function(pc, loads, .color_varname){
    color_varname <- ensym(.color_varname)
    
    # we don't care about absolute variable loadings, so scale these to be interpretable
    const <- var(pc$PC1) / max(loads$PC1)
    loads %<>% mutate_at(vars(matches("PC")), ~ .x * const)

    pc %>% ggplot() %>%
        + geom_point(aes(PC1, PC2, colour=factor(!!color_varname))) %>%
        + scale_color_hue(name="Cluster") %>%
        + geom_segment(data=loads,
                       aes(x=0, y=0, xend=PC1, yend=PC2),
                       colour='red', arrow=arrow(type='open')) %>%
        + geom_text(data=loads, aes(PC1, PC2, label=var)) %>%
        print
}
pca.loadings.plot(pc.data, load.dat, cluster)

pca.loadings.plot(pc.data, load.dat, outcome)


