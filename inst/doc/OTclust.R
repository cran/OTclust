## ----echo=F, results="asis"----------------------------------------------
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

## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=T, eval=T-----------------------------------------------------
library(OTclust)
data(sim1)

## ---- echo=F, eval=F, results="hide", cache=F----------------------------
#  C=4
#  load('ens.data.rda')
#  load('OTA.rda')

## ---- fig.show='hold', cache=T-------------------------------------------
# CPS Analysis on validation of clustering result
data(YAN)
y=clustCPS(YAN, k=7, l=FALSE, pre=FALSE, noi="after", cmethod="kmeans", dimr="PCA", vis="tsne")

# visualization of the results
mplot(y,4)
cplot(y,4)

# point-wise stability assessment
p=pplot(y)
p$v

