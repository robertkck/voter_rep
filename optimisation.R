# Optimisation exercise
# author: Robert Kalcik

# This script calculates the Cambridge Compromise allocation of seats for
# Parliament sizes between 600 and 800 seats. The allocation is subsequently
# evaluated using a number of measures of inequality. The results are plotted.

library(ggplot2)
library(countrycode)
library(SciencesPo)
library(tidyverse)
source('funk/voting_gini.R')
source('funk/camcom.R')
source('funk/malapportionment.R')

# Load population data and set treaty limits
eu <- read.csv("data/eu.csv")
eu <- eu[eu$GEO != "United Kingdom", ]
m <- 6
M <- 96

# Calculate CamCom allocation for every Parliament size between 600 and 800
rep_opt <- purrr::map(600:800, ~ alloc.camcom(eu$pop, m, M, .x)$rep)
rep_opt_df <- data.frame(matrix(unlist(rep_opt), nrow = 27), row.names = eu$GEO)
colnames(rep_opt_df) <- paste0('H', 600:800)
write.csv(rep_opt_df, 'output/CamCom_allocations.csv')

# Compute Gini, Malapportionment and other inequality metrics from the Science Po package
ginis <- map_dbl(rep_opt, ~ voting_gini(eu$pop, .x))
mals <- map_dbl(rep_opt, ~ mal(eu$pop, .x))
spo1 <- map_dbl(rep_opt, ~ SciencesPo::Proportionality(eu$pop, .x, index = "Loosemore-Hanby"))
spo2 <- map_dbl(rep_opt, ~ 10 * Proportionality(eu$pop, .x, index = "Rae"))
spo3 <- map_dbl(rep_opt, ~ 1 - Proportionality(eu$pop, .x, index = "Cox-Shugart"))
spo4 <- map_dbl(rep_opt, ~ Proportionality(eu$pop, .x, index = "Farina"))
spo5 <- map_dbl(rep_opt, ~ Proportionality(eu$pop, .x, index = "Gallagher"))
spo6 <- map_dbl(rep_opt, ~ 10 * Proportionality(eu$pop, .x, index = "Grofman"))
spo7 <- map_dbl(rep_opt, ~ Proportionality(eu$pop, .x, index = "Lijphart"))
spo8 <- map_dbl(rep_opt, ~ 1 - Proportionality(eu$pop, .x, index = "Rose"))
spo9 <- map_dbl(rep_opt, ~ Proportionality(eu$pop, .x, index = "DHondt"))

# Collect and demean results
results <- data.frame(Gini = ginis, "LoosemoreHanby" = mals, Rae = spo2, Cox = spo3, Farina = spo4, Gallagher = spo5, Grofman = spo6, Rose = spo8)
results <-  sweep(results, 2, apply(results, 2, mean))
minima <- apply(results, 2, which.min)
results$H <- 600:800

# Export the long format
results_long <- results %>%
  gather("methods", "t", -H) %>%
  transform(methods=factor(methods,levels=c("Gini", "Cox", "Farina", "Gallagher", "LoosemoreHanby", "Grofman", "Rae", "Rose")))
write.csv(results_long, 'optimisation.csv')

# Prepare facet plot
min_labels <- data.frame(x = 650, y = 0.04, min_text = paste0("Min: ", 600 + minima), methods = names(minima))
p <- ggplot(results_long, aes(x = H, y = t, color = methods)) +
      geom_point() +
      facet_wrap( ~ methods, ncol = 4) +
      scale_x_continuous(breaks = c(600, 700, 800)) +
      geom_text(data = min_labels, aes(x,y,label=min_text, color = methods),  color = "black", inherit.aes=FALSE) +
      xlab("Parliament size") +
      ylab("Demeaned inequality coefficients") +
      theme(legend.position="none")
p


