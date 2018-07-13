## ---- echo=F, message=FALSE, warning=FALSE-------------------------------
library(knitr)
library(dplyr)
library(ggplot2)
library(rstanarm)

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

## ---- out.width=700, echo = FALSE, eval = TRUE, fig.align='center'-------
knitr::include_graphics("images/workflow.PNG")

## ----eval=TRUE, fig.align='center', fig.height=4.5, fig.width=9, message=FALSE, warning=FALSE, results='markup'----
library(psycho)

# Let's create a correlation plot
p <- plot(psycho::correlation(iris))

# Custom theme and colours
p <- p +
  scale_fill_gradientn(colors = c("#4CAF50", "#FFEB3B", "#FF5722")) +
  ylab("Variables\n") +
  labs(fill = "r") +
  theme(
    plot.background = element_rect(fill = "#607D8B"),
    axis.title.y = element_text(size = 20, angle = 90, colour = "white"),
    axis.text = element_text(size = 15, colour = "white"),
    legend.title = element_text(size = 20, colour = "white"),
    legend.text = element_text(size = 15, colour = "white"),
    title = element_text(size = 16, colour = "white")
  )
p

## ----echo=TRUE, message=FALSE, warning=FALSE, results='markup'-----------
library(psycho)

patient <- 61 # The IQ of a patient
controls <- c(86, 100, 112, 95, 121, 102) # The IQs of a control group

result <- crawford.test(patient, controls)
print(result)
plot(result)

## ----echo=TRUE, message=FALSE, warning=FALSE, results='markup'-----------
library(psycho)

case_X <- 132
case_Y <- 7
controls_X <- c(100, 125, 89, 105, 109, 99)
controls_Y <- c(7, 8, 9, 6, 7, 10)

result <- crawford_dissociation.test(case_X, case_Y, controls_X, controls_Y)

## ----echo=TRUE, message=FALSE, warning=FALSE, results='markup'-----------
library(psycho)

t0 <- 82 # The IQ of a patient at baseline
t1 <- 105 # The IQ of a patient after the new therapy
controls <- c(94, 100, 108, 95, 102, 94) # The IQs of a control group

rez <- mellenbergh.test(t0, t1, controls = controls)

# if we do not have a control group, we can also directly enter the SD of the score.
# For IQ, the SD is of 15.
rez <- mellenbergh.test(t0, t1, controls = 15)

