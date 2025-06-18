#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2025-06-18
# ~: benchmarks of similarity algorithms
#------------------------------------------------------------------------------#


####
#### benchmark: algo 1 vs algo 2 ####
####

# 200 abstracts
abstracts = readLines("./_DATA/pub-abstracts.txt")

base_1 = jaccard_algo_1(abstracts)
#> Elapsed:      5s 546ms 444us

base_2 = jaccard_algo_2(abstracts)
#> Elapsed:           6ms 544us

all(sort(base_1$jaccard) == sort(base_2$jaccard))
# [1] TRUE

####
#### benchmark: algo 2 vs algo chatGPT ####
####

# 200 abstracts
abstracts = readLines("./_DATA/pub-abstracts.txt")

res1 = jaccard_algo_2(abstracts)
#> Elapsed:          91ms 512us

res2 = jaccard_algo_chatgpt(abstracts)
#> Elapsed:      9s 626ms 168us 


####
#### benchmark: algo 2 vs algo chatGPT faster ####
####

# 200 abstracts
abstracts = readLines("./_DATA/pub-abstracts.txt")

res1 = jaccard_algo_2(abstracts)
#> Elapsed:           78ms 544us

res2 = jaccard_algo_chatgpt_faster(abstracts)
#> Elapsed:         472ms 554us 

# does it scale? Let's check with 400 abstracts
double_abstracts = c(abstracts, abstracts)

res1 = jaccard_algo_2(double_abstracts)
#> Elapsed:          96ms 888u

res2 = jaccard_algo_chatgpt_faster(double_abstracts)
#> Elapsed:      1s 952ms 263us 

####
#### benchmark: algo 2 vs algo  ####
####

# 200 abstracts
abstracts = readLines("./_DATA/pub-abstracts.txt")

res1 = jaccard_algo_2(abstracts)
#> Elapsed:           77ms 378us

res2 = jaccard_algo_chatgpt_fastest(abstracts)
#> Error in .set_ops_arg_check(x, y, all, .seqn = TRUE) : 
#>  x and y must both be data.tables


