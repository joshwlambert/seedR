test_seed <- function(num_samples) {
  cors_vec <- c()
  cors_vec_jump <- c()
  for (i in 1:1000) {
    message("Replicate ", i, "of 1000")
    set.seed(i)
    sample_1e3_1 <- runif(num_samples)

    DAISIEutils::jump_seed(1)
    sample_1e3_2 <- runif(num_samples)

    cor_1e3_jump <- cor(sample_1e3_1, sample_1e3_2)
    cors_vec_jump[i] <- cor_1e3_jump

    set.seed(i)
    sample_1e3_1 <- runif(num_samples)
    set.seed((i + 1))
    sample_1e3_2 <- runif(num_samples)

    cor_1e3 <- cor(sample_1e3_1, sample_1e3_2)
    cors_vec[i] <- cor_1e3
  }
return(list(
  cors_vec = cors_vec,
  cors_vec_jump = cors_vec_jump))
}


