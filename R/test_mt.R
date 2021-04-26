#' Tests the correlation using Spearman's rank between samples using different
#' Mersenne-Twister seed setups.
#'
#' @param num_samples stub
#' @param reps stub
#'
#' @return List of correlation for different seed setups
#' @export
#'
#' @examples
#' \dontrun{
#' test_mt(num_samples = 10, reps = 100)
#' }
test_mt <- function(
  num_samples,
  reps) {

  cors_vec <- c()
  cors_vec_jump <- c()
  for (i in seq_len(reps)) {
    message("Replicate ", i, " of ", reps)
    set.seed(
      i,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )

    sample_1 <- stats::runif(
      n = num_samples,
      min = 0,
      max = 1
    )
    jump_seed(1)
    sample_2 <- stats::runif(
      n = num_samples,
      min = 0,
      max = 1
    )

    cor_jump <- stats::cor(
      sample_1,
      sample_2,
      method = "spearman")
    cors_vec_jump[i] <- cor_jump

    set.seed(
      i,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    sample_1 <- stats::runif(
      n = num_samples,
      min = 0,
      max = 1
    )
    set.seed(
      (i + 1),
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    sample_2 <- stats::runif(
      n = num_samples,
      min = 0,
      max = 1
    )

    cor <- stats::cor(
      sample_1,
      sample_2,
      method = "spearman"
    )
    cors_vec[i] <- cor
  }
  return(list(
    cors_vec = cors_vec,
    cors_vec_jump = cors_vec_jump))
}
