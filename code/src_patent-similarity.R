#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2025-06-16
# ~: functions to compute text similarity
#------------------------------------------------------------------------------#

sim_single = function(x, y){
  words_x = strsplit(x, " ")[[1]]
  words_y = strsplit(y, " ")[[1]]

  sim = length(intersect(words_x, words_y)) / length(union(words_x, words_y))
  
  return(sim)
}

jaccard_algo_1 = function(x){
  #  in: vector of text
  # out: data.frame with i,j, jaccard measure
  
  n = length(x)
  base_pairs = expand.grid(words_i = x, words_j = x, stringsAsFactors = FALSE)
  
  pairs_id = expand.grid(i = 1:n, j = 1:n)

  base_pairs = within(base_pairs, {
    i = pairs_id$i
    j = pairs_id$j
  })

  base_pairs = subset(base_pairs, i != j)

  n_pairs = nrow(base_pairs)
  sim = numeric(n_pairs)
  for(index in 1:n_pairs){
    sim[index] = sim_single(base_pairs$words_i[index], base_pairs$words_j[index])
  }

  base_pairs$jaccard = sim
  
  return(base_pairs)
}

jaccard_algo_2 = function(x){
  #  in: x, text vector
  # out: data frame with i, j, jaccard
  
  library(data.table)
  
  # we split the sentences into words
  n = length(x)
  x_split = strsplit(x, "\\s+")
  n_all = lengths(x_split)
  
  # we create the id x word data set
  base_word_i = data.table(i = rep(1:n, n_all), word = unlist(x_split))
  setkey(base_word_i, word)
  base_word_i = unique(base_word_i)
  
  base_word_j = setNames(base_word_i, c("j", "word"))
  
  # word, i, j data set
  base_word_ij = merge(base_word_i, base_word_j, allow.cartesian = TRUE)
  
  # we get the numerator
  base_sim = base_word_ij[, .(numerator = .N), by = .(i, j)]

  # we add the two sizes
  base_size_i = base_word_i[, .(size_i = .N), by = i]
  
  base_sim = merge(base_sim, base_size_i, by = "i")
  base_sim = merge(base_sim, base_size_i[, .(j = i, size_j = size_i)], by = "j")
  
  # compute the denom and the measure
  base_sim[, denom := size_i + size_j - numerator]
  base_sim[, jaccard := numerator / denom]
  
  # final step: adding numerator = 0
  res = as.data.table(expand.grid(i = 1:n, j = 1:n))
  res = res[i != j]
  res = merge(res, base_sim[, .(i, j, jaccard)], by = c("i", "j"), all.x = TRUE)
  res[is.na(jaccard), jaccard := 0]
  
  return(res)
}


jaccard_algo_chatgpt <- function(sentences) {
  # Tokenize sentences into lowercase word sets
  word_sets <- lapply(sentences, function(s) {
    words <- tolower(s)
    words <- unlist(strsplit(words, "\\s+"))
    return(unique(words))
  })
  
  # Prepare result container
  results <- data.frame(i = integer(), j = integer(), sim = numeric())
  
  n <- length(sentences)
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      words_i <- word_sets[[i]]
      words_j <- word_sets[[j]]
      
      common <- intersect(words_i, words_j)
      total <- union(words_i, words_j)
      
      sim <- length(common) / length(total)
      
      results <- rbind(results, data.frame(i = i, j = j, sim = sim))
    }
  }
  
  return(results)
}


jaccard_algo_chatgpt_faster <- function(sentences) {
  
  # Preprocess: convert to lowercase and split into unique word sets
  word_sets <- lapply(sentences, function(s) {
    unique(strsplit(tolower(s), "\\s+")[[1]])
  })
  
  n <- length(sentences)
  n_pairs <- choose(n, 2)
  
  # Preallocate result list
  results <- vector("list", n_pairs)
  idx <- 1
  
  for (i in 1:(n - 1)) {
    words_i <- word_sets[[i]]
    for (j in (i + 1):n) {
      words_j <- word_sets[[j]]
      
      common <- intersect(words_i, words_j)
      total <- union(words_i, words_j)
      sim <- length(common) / length(total)
      
      results[[idx]] <- c(i, j, sim)
      idx <- idx + 1
    }
  }
  
  # Convert to data.frame
  results_df <- as.data.frame(do.call(rbind, results))
  names(results_df) <- c("i", "j", "sim")
  
  # Convert column types
  results_df$i <- as.integer(results_df$i)
  results_df$j <- as.integer(results_df$j)
  results_df$sim <- as.numeric(results_df$sim)
  
  return(results_df)
}



jaccard_algo_chatgpt_fastest <- function(sentences) {
  
  library(data.table)
  
  n <- length(sentences)
  
  # Tokenize each sentence into a list of unique lowercase words
  tokens <- lapply(sentences, function(s) {
    unique(strsplit(tolower(s), "\\s+")[[1]])
  })
  
  # Create all unique pairs of sentence indices
  pairs <- CJ(i = 1:n, j = 1:n)[i < j]
  
  # Compute similarity for each pair
  pairs[, sim := mapply(function(i, j) {
    words_i <- tokens[[i]]
    words_j <- tokens[[j]]
    length(fintersect(words_i, words_j)) / length(funion(words_i, words_j))
  }, i, j)]
  
  return(pairs[])
}



