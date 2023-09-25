## ----echo=F, results="asis"---------------------------------------------------
cat("
<style>
samp {
   color: red;
   background-color: #EEEEEE;
}
</style>
")

cat("
<style>
samp2 {
   color: black;
   font-style: italic;
   background-color: #EEEEEE;
}
</style>
")

## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=T, eval=T-----------------------------------------------------------
library(OTclust)
data(sim1)

## ----echo=F, eval=F, results="hide", cache=F----------------------------------
#  C=4
#  load('ens.data.rda')
#  load('OTA.rda')

## ----echo=T, eval=T, results="hide", cache=T----------------------------------
# the number of clusters.
C = 4
# generate an ensemble of perturbed partitions.
# if perturb_method is 1 then perturbed by bootstrap resampling, it it is 0, then perturbed by adding Gaussian noise.
ens.data = ensemble(sim1$X, nbs=100, clust_param=C, clustering="kmeans", perturb_method=1)

## ----echo=T, eval=T, results="hide", cache=T----------------------------------
# find mean partition and uncertainty statistics.
ota = otclust(ens.data)

## ----echo=T, cache=T----------------------------------------------------------
# calculate baseline method for comparison.
kcl = kmeans(sim1$X,C)

# align clustering results for convenience of comparison.
compar = align(cbind(sim1$z,kcl$cluster,ota$meanpart))
lab.match = lapply(compar$weight,function(x) apply(x,2,which.max))
kcl.algnd = match(kcl$cluster,lab.match[[1]])
ota.algnd = match(ota$meanpart,lab.match[[2]])

## ----echo=T, cache=T, fig.show='hold'-----------------------------------------
# plot the result on two dimensional space.
otplot(sim1$X,sim1$z,con=F,title='Truth')   # ground truth
otplot(sim1$X,kcl.algnd,con=F,title='Kmeans')   # baseline method
otplot(sim1$X,ota.algnd,con=F,title='Mean partition')   # mean partition by OTclust

## ----echo=T, cache=T----------------------------------------------------------
# distance between ground truth and each partition
wassDist(sim1$z,kmeans(sim1$X,C)$cluster)   # baseline method
wassDist(sim1$z,ota$meanpart)   # mean partition by OTclust

# Topological relationships between mean partition and ensemble clusters
t(ota$match)

# Cluster Alignment and Points based (CAP) separability
ota$cap

## ----echo=T, cache=T, fig.show='hold'-----------------------------------------
# Covering Point Set(CPS)
otplot(sim1$X,ota$cps[lab.match[[2]][1],],legend.labels=c('','CPS'),add.text=F,title='CPS for C1')
otplot(sim1$X,ota$cps[lab.match[[2]][2],],legend.labels=c('','CPS'),add.text=F,title='CPS for C2')
otplot(sim1$X,ota$cps[lab.match[[2]][3],],legend.labels=c('','CPS'),add.text=F,title='CPS for C3')
otplot(sim1$X,ota$cps[lab.match[[2]][4],],legend.labels=c('','CPS'),add.text=F,title='CPS for C4')

## ----fig.show='hold', cache=T-------------------------------------------------
# CPS analysis on selection of visualization methods
data(vis_pollen)
c=visCPS(vis_pollen$vis, vis_pollen$ref)

## ----fig.show='hold', cache=T-------------------------------------------------
# visualization of the result
mplot(c,2)
cplot(c,2)

## ----fig.show='hold', cache=T-------------------------------------------------
# overall tightness
c$tight_all
# cluster-wise tightness
c$tight

## ----fig.show='hold', cache=T-------------------------------------------------
# CPS Analysis on validation of clustering result
data(YAN)
y=clustCPS(YAN, k=7, l=FALSE, pre=FALSE, noi="after", cmethod="kmeans", dimr="PCA", vis="tsne")

# visualization of the results
mplot(y,4)
cplot(y,4)

# point-wise stability assessment
p=pplot(y)
p$v

