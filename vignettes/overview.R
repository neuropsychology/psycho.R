## ---- echo=F, message=FALSE, warning=FALSE-------------------------------
library(knitr)
library(dplyr)
library(ggplot2)
library(rstanarm)

## ---- out.width=700, echo = FALSE, eval = TRUE, fig.align='center'-------
knitr::include_graphics("images/workflow.PNG")

## ---- eval = FALSE-------------------------------------------------------
#  # This for the stable version:
#  install.packages("psycho")
#  
#  # Or this for the dev version:
#  install.packages("devtools")
#  library(devtools)
#  devtools::install_github("neuropsychology/psycho.R")

## ------------------------------------------------------------------------
library(psycho)

