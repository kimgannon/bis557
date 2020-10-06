## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bis557)
library(palmerpenguins)
library(DT)
data("penguins")
datatable(penguins)


## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

#The "base" way
table(penguins$species, penguins$island)

#The "tidy" way
penguins %>%
  select(species, island) %>% 
  table()

## -----------------------------------------------------------------------------
#Base
adelies <- penguins[penguins$species == "Adelie",]
table(adelies$sex)

#Tidy
penguins %>%
  filter(species == "Adelie") %>%
  select(sex) %>%
  table()

## -----------------------------------------------------------------------------
fit <- lm(bill_length_mm ~ species + island + sex, data=penguins)
summary(fit)


## -----------------------------------------------------------------------------
#base
qs <- qqnorm(fit$residuals, plot.it = FALSE)
qsd <- as.data.frame(qs)
ggplot(qsd, aes(x=x, y=y)) + geom_point() + xlab("Theoretical Quantiles") + ylab("Sample Quantiles") + theme_minimal()

#tidy 
qs <- qqnorm( fit$residuals, plot.it = FALSE) %>%
  as_tibble() %>%
  ggplot(aes(x=x, y=y)) + geom_point() + xlab("Theoretical Quantiles") + ylab("Sample Quantiles") + theme_minimal()

