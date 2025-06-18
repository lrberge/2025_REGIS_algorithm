#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2025-06-11
# ~: weighted fixed-effects
#------------------------------------------------------------------------------#

# For more details, see 
# https://github.com/lrberge/fixest/blob/dbe56c5d953deb797a61dfc37f6e25fd143c68d4/_Code%20snippets/fixest_snippets.R#L384
# 


## Packages to install (only if not already installed)
# install.packages("wfe")
# install.packages("fixest")
# install.packages("data.table")


####
#### New unit-weights algorithm ####
####

unit_weights = function(index){
  # index: data.frame with two columns: unit and treatment indicator (treat x post)
  
  # for easy aggregations
  library(data.table)
  
  res = as.data.table(index)
  names(res) = c("unit", "treatment")
  
  # we compute the weigts as in Theorem 1 (p. 427)
  res[, n_treated := sum(treatment), by = unit]
  res[, n_not_treated := sum(1 - treatment), by = unit]
  res[, m_size := treatment * n_not_treated + (1 - treatment) * n_treated]
  res[, w := 1 + treatment * (m_size / n_treated) + (1 - treatment) * (m_size / n_not_treated)]
  res[is.na(w), w := 0]
  
  # we return the weights
  res$w
}


####
#### Comparing the estimators ####
####

  library(wfe)
  library(fixest)
  library(data.table)

  # example data set
  data(base_did, package = "fixest")
  set.seed(0)
  base_example = as.data.table(base_did)
  id_first = sample(108, 20) ; id_last = sample(108, 20)
  base_example = base_example[!(id %in% id_first & period <= 3) & !(id %in% id_last & period >= 7)]
  base_example[, treat_post := treat*post]

# wfe V1 vs V2
microbenchmark(
  wfe_v1 = wfe(y ~ treat_post + x1, data = base_example, treat = "treat_post", 
               unit.index = "id", time.index = "period", method = "unit",
               qoi = "ate", hetero.se = TRUE, auto.se = TRUE), 
  
  wfe_v2 = feols(y ~ treat_post + x1 | id, base_example, 
                 weights = unit_weights(base_example[, .(id, treat_post)])),
  times = 4)
#> Unit: milliseconds
#>    expr      min       lq      mean    median        uq      max neval
#>  wfe_v1 119.9691 122.8909 125.53608 126.92805 128.18125 128.3191     4
#>  wfe_v2  10.4454  11.3273  11.79133  12.21255  12.25535  12.2948     4


