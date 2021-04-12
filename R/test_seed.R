#' Tests the correlation using Spearman's rank between samples using differnt
#' seed setups.
#'
#' @param num_samples stub
#' @param reps stub
#'
#' @return List of correlation for different seed setups
#' @export
#'
#' @examples
#' \dontrun{
#' test_seed(10, 100)
#' }
test_seed <- function(
  num_samples,
  reps) {

  cors_vec <- c()
  cors_vec_jump <- c()
  for (i in seq_len(reps)) {
    message("Replicate ", i, " of ", reps)
    set.seed(i)
    sample_1e3_1 <- stats::runif(
      n = num_samples,
      min = 0,
      max = 1
    )
    jump_seed(1)
    sample_1e3_2 <- stats::runif(
      n = num_samples,
      min = 0,
      max = 1
    )

    cor_1e3_jump <- stats::cor(
      sample_1e3_1,
      sample_1e3_2,
      method = "spearman")
    cors_vec_jump[i] <- cor_1e3_jump

    set.seed(i)
    sample_1e3_1 <- stats::runif(
      n = num_samples,
      min = 0,
      max = 1
    )
    set.seed((i + 1))
    sample_1e3_2 <- stats::runif(
      n = num_samples,
      min = 0,
      max = 1
    )

    cor_1e3 <- stats::cor(
      sample_1e3_1,
      sample_1e3_2,
      method = "spearman"
    )
    cors_vec[i] <- cor_1e3
  }
  return(list(
    cors_vec = cors_vec,
    cors_vec_jump = cors_vec_jump))
}
