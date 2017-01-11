
library(ggplot2)
library(countrycode)
library(SciencesPo)
library(tidyverse)
source('funk/voting_gini.R')
source('funk/camcom.R')
source('funk/parabolic.R')
source('funk/limitloss.R')
source('funk/malapportionment.R')

eu_brexit <- read.csv("data/eu_brexit.csv")
eu_brexit$ctry <- countrycode(eu_brexit$GEO, "country.name", "iso3c")
eu <- read.csv("data/eu.csv")
eu$ctry <- countrycode(eu$GEO, "country.name", "iso3c")
meps <- read.csv("data/meps.csv")
# meps_mat <- table(meps$country, meps$grp)
meps_table <- count(meps, country, grp)
meps_table[(meps_table$country == "Spain") & (meps_table$grp == "EPP"), 'n'] <- 17
meps_table <- spread(meps_table, grp, n, fill=0)
meps_table[,-1] <- meps_table[,-1] / rowSums(meps_table[,-1])
meps_group <- count(meps, grp, group)
meps_group[(meps_group$grp == "EPP"), 'n'] <- 217
# m_short <- meps_group$grp
# m_long <- meps_group$group
m_labels <- data.frame(meps_group$grp, meps_group$group, colors)

# Merge data sets
eu <- merge(meps_table, eu, by.x="country", by.y = "GEO")
eu <- eu[order(-eu$pop),]
eu_brexit <- merge(meps_table, eu_brexit, by.x="country", by.y = "GEO")
eu_brexit <- eu_brexit[order(-eu_brexit$pop),]

# Defaults
data <- eu_brexit
data <- select(
  data, country, ALDE, ECR, EFDD, ENF, EPP,
  contains("Greens"), contains("GUE/NGL"), NI, contains("S&D"),
  ctry, pop, rep,  pop_share, pop_rep, rep_share
)
data$rep_scen <- data$rep
m <- 6
M <- 96
H <- 751

rep_opt <- as.list(NULL)
for (i in 1:400){
  H <- 550 + i
  out <- alloc.camcom(data$pop, m, M, H)
  rep_opt[[i]] <- out$rep
}

# saveRDS(rep_opt, "rep_opt.rds")

ginis <- 1
mals <- 1
spo1 <- 1
spo2 <- 1
spo3 <- 1
spo4 <- 1
spo5 <- 1
spo6 <- 1
spo7 <- 1
spo8 <- 1
spo9 <- 1

for (i in 50:250){
  rep_share <- rep_opt[[i]] / sum(rep_opt[[i]])
  ginis <- c(ginis, voting_gini(data$pop_share, rep_share))
  mals <- c(mals, mal(data$pop_share, rep_share))
  spo1 <- c(spo1, Proportionality(data$pop_share, rep_share, index = "Loosemore-Hanby"))
  spo2 <- c(spo2, 10 * Proportionality(data$pop_share, rep_share, index = "Rae"))
  spo3 <- c(spo3, 0.9 - Proportionality(data$pop_share, rep_share, index = "Cox-Shugart"))
  spo4 <- c(spo4, Proportionality(data$pop_share, rep_share, index = "Farina"))
  spo5 <- c(spo5, Proportionality(data$pop_share, rep_share, index = "Gallagher"))
  spo6 <- c(spo6, 10 * Proportionality(data$pop_share, rep_share, index = "Grofman"))
  spo7 <- c(spo7, 10 * Proportionality(data$pop_share, rep_share, index = "Lijphart"))
  spo8 <- c(spo8, 1 - Proportionality(data$pop_share, rep_share, index = "Rose"))
  spo9 <- c(spo9, Proportionality(data$pop_share, rep_share, index = "DHondt"))
}
results <- data.frame(Gini = ginis, "LoosemoreHanby" = mals, Rae = spo2, Cox = spo3, Farina = spo4, Gallagher = spo5, Grofman = spo6, Rose = spo8) # spo9, spo7, spo1
results <- results[-1,]
minima <- apply(results, 2, which.min)
results <- sweep(results, 2, apply(results, 2, mean))
results$x <- 600:800
results_long <- gather(results, "methods", "t", -x)
results_long <- transform(results_long, methods=factor(methods,levels=c("Gini", "Cox", "Farina", "Gallagher", "LoosemoreHanby", "Grofman", "Rae", "Rose")))
min_labels <- data.frame(x = 650, y = 0.04, min_text = paste0("Min: ", 600 + minima), methods = names(minima))

p <- ggplot(results_long, aes(x, t)) +
      geom_point(aes(color = methods)) +
      facet_wrap( ~ methods, ncol = 4) +
      scale_x_continuous(breaks = c(600, 700, 800)) +
      geom_text(data = min_labels, aes(x,y,label=min_text, color = methods),  color = "black", inherit.aes=FALSE) +
      xlab("Parliament size") +
      ylab("Demeaned inequality coefficients") +
      theme(legend.position="none")
p


