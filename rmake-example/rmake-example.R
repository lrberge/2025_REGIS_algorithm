#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2025-06-12
# ~: basic code to illustrate rmake
#------------------------------------------------------------------------------#


####
#### = main data set ####
####

set.seed(1)

n = 200
base = data.frame(x = rnorm(n))
base$y = 2 + 0.5 * base$x + rnorm(n)

saveRDS(base, file = "_DATA/data-example.rds")

####
#### = OLS estimation ####
####

base = readRDS("_DATA/data-example.rds")

library(fixest)

est = feols(y ~ x, base)
etable(est, export = "images/EST_yx.jpeg")



