#' @import microbenchmark
#' @import xtable
#' @import MASS
#' @import boot
#' @import bootstrap
#' @import DAAG
#' @import coda
#' @import lpSolve
#' @import ggplot2
#' @import reshape2
#' @import stats
#' @importFrom Rcpp evalCpp
#' @importFrom stats rnorm rgamma
#' @useDynLib SC24002027
NULL

#' @title Compute Bootstrap Uncertainty and Confidence Intervals
#' @description This function calculates the standard error and confidence intervals for each pixel in a 3D 
#' array of image samples using the bootstrap method.
#' @param original_samples A 3D array of image samples.
#' @param n_bootstrap The number of bootstrap samples to generate.
#' @param ci_level The confidence level for the intervals.
#' @return A list containing the standard error matrix, lower confidence interval matrix, 
#' and upper confidence interval matrix.
#' @examples
#' \dontrun{
#' image_dim <- 10
#' original_samples <- array(rnorm(1000 * image_dim^2, mean = 0.5, sd = 0.2), 
#' dim = c(1000, image_dim, image_dim))
#' result <- bootstrap_uncertainty_with_ci(original_samples, n_bootstrap = 500, ci_level = 0.95)
#' se_matrix <- result$standard_error
#' lower_matrix <- result$ci_lower
#' upper_matrix <- result$ci_upper
#' cat("置信区间 (95%) for pixel (1, 1): [", lower_matrix[1, 1], ", ", upper_matrix[1, 1], "]\n")
#' se_df <- melt(se_matrix)
#' lower_df <- melt(lower_matrix)
#' upper_df <- melt(upper_matrix)
#' ggplot(melt(se_matrix), aes(x = Var1, y = Var2, fill = value)) +
#' geom_tile() +
#'   scale_fill_gradient(low = "blue", high = "red") +
#'   labs(title = "Standard Error Map", x = "X", y = "Y") +
#'   theme_minimal()
#' ci_length <- upper_matrix - lower_matrix
#' ggplot(melt(ci_length), aes(x = Var1, y = Var2, fill = value)) +
#'   geom_tile() +
#'   scale_fill_gradient(low = "blue", high = "red") +
#'   labs(title = "Confidence Interval Length", x = "X", y = "Y") +
#'   theme_minimal()
#' }
#' @export
bootstrap_uncertainty_with_ci <- function(original_samples, n_bootstrap = 1000, ci_level = 0.95) {
  img_height <- dim(original_samples)[2]
  img_width <- dim(original_samples)[3]
  n_samples <- dim(original_samples)[1]
  
  se_matrix <- matrix(0, nrow = img_height, ncol = img_width)
  lower_matrix <- matrix(0, nrow = img_height, ncol = img_width)
  upper_matrix <- matrix(0, nrow = img_height, ncol = img_width)
  
  for (i in 1:img_height) {
    for (j in 1:img_width) {
      pixel_values <- original_samples[, i, j]
      
      bootstrap_means <- replicate(n_bootstrap, {
        sample_values <- sample(pixel_values, size = n_samples, replace = TRUE)
        mean(sample_values)
      })
      
      se_matrix[i, j] <- sd(bootstrap_means)
      
      lower_matrix[i, j] <- quantile(bootstrap_means, probs = (1 - ci_level) / 2)
      upper_matrix[i, j] <- quantile(bootstrap_means, probs = 1 - (1 - ci_level) / 2)
    }
  }
  
  return(list(
    standard_error = se_matrix,
    ci_lower = lower_matrix,
    ci_upper = upper_matrix
  ))
}