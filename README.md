# Voter representation project and Shiny app

<!-- badges: start -->
[![Launch Rstudio Binder](http://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/robertkck/voter_rep/master?urlpath=rstudio)
<!-- badges: end -->


This repo contains data and code for the Bruegel publication ['Is Brexit an Opportunity to Reform the European Parliament'](http://bruegel.org/2017/01/is-brexit-an-opportunity-to-reform-the-european-parliament/) and is the basis for the accompanying [Shiny app](https://robertkck.shinyapps.io/voter_rep/).

## Code
The code for the project is split in 
* code that is used for the Shiny app: ui.R and server.R,
* functions to calculate allocations of MEPs and measures of inequality in the 'funk' folder and
* code that is used for exploratory analysis and the optimisation exercise in 'vrep.py' and 'optimisation.R' respectively.

## Data
The data folder includes three CSVs:
* eu.csv contains information on population at the end of 2016, number of MEPs and a few derived variables. It serves as the main input to the Shiny app.
* eu_brexit.csv is the result of an exploratory analysis performed in 'vrep.py' including a number of scenarios.
* meps.csv is a list of all MEPs in the EP and their country and political group affiliation.

## Next steps
Following the publication of the policy contribution and Shiny app, code and data will be bundled in an R package. 

A number of improvements to the Shiny app are planned including object constancy.