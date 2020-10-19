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

