#' Permutation test for comparing means between two groups
#'
#' This function conducts a permutation test to compare the means of two groups.
#' It calculates the observed difference in means between group A and group B,
#' generates permuted samples to simulate the null distribution, and computes
#' the p-value based on the proportion of permuted samples that exhibit a
#' difference in means as extreme as or more extreme than the observed difference.
#'
#' @param x A numeric vector representing group A
#' @param y A numeric vector representing group B
#' @param B Number of permutations to perform (default = 1000)
#'
#' @return p-value indicating the significance of the observed difference
#' @export
#'
#' @examples
#' # Example usage:
#' # x <- c(1.2, 3.5, 2.8, 4.1)
#' # y <- c(2.5, 1.8, 3.2)
#' # permutation_test(x, y)
permutation_test <- function(x, y, B = 1000) {
  set.seed(1)
  orig_diff <- abs(mean(x) - mean(y))
  diffs <- replicate(B, {
    perm <- sample(c(x, y), length(x) + length(y), replace = FALSE)
    abs(mean(perm[1:length(x)]) - mean(perm[(length(x)+1):(length(x)+length(y))]))
  })
  pval <- sum(diffs >= orig_diff) / B
  return(pval)
}
