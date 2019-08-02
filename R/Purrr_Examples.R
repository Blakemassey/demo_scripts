dataset <- replicate(5, data.frame(x1 = 1:5, x2 = letters[1:5]), simplify = F)
tbl_df <- tibble(dataset, sample_size = 1:5)

sample_function1 <- function(dataset, sample_size){
  samp1 <- sample(dataset[,1], sample_size)
  samp2 <- sample(dataset[,2], sample_size)
  samp12 <- paste0(samp1, samp2)
  samp_rep <- rep(samp12, length.out = 5)
  samp_out <- paste0(samp_rep, collapse = ".")
  return(samp_out)
}

sample_function2 <- function(dataset, sample_size){
  samp1 <- sample(dataset[,1], sample_size)
  samp2 <- sample(dataset[,2], sample_size)
  samp12 <- paste0(samp1, samp2)
  samp_rep <- rep(samp12, length.out = 5)
  samp_out <- paste0(samp_rep, collapse = ".")
  samp_out2 <- paste0(samp_rep, collapse = ",")
  return(list(result1 = samp_out, result2 = samp_out2))
}

sample_function_df <- function(dataset, sample_size){
  samp1 <- sample(dataset[,1], sample_size)
  samp2 <- sample(dataset[,2], sample_size)
  samp12 <- paste0(samp1, samp2)
  samp_rep <- rep(samp12, length.out = 5)
  samp_out <- paste0(samp_rep, collapse = ".")
  samp_out2 <- paste0(samp_rep, collapse = ",")
  return(data.frame(result1 = samp_out, result2 = samp_out2))
}

# Results only
tbl_df %>%
  pmap(sample_function1)

# Results (character) inserted into tbl_df as a list column
tbl_df %>%
  mutate(result = pmap(., sample_function1))

# Results (df) inserted into tbl_df as a list column
tbl_df %>%
  mutate(result = pmap(., sample_function_df))

# Results (character) inserted into tbl_df as character columns
tbl_df %>%
  mutate(result = pmap_chr(., sample_function1))

# Results (two character vectors) inserted in tbl_df as two columns
tbl_df %>%
  bind_cols(pmap_df(., sample_function2))
