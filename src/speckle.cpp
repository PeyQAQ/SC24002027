#include <Rcpp.h>
#include <cmath>
#include <random>
using namespace Rcpp;

//' @title Compute Target Density for Multimodal Distribution
//' @description This function computes the target density for a given value x, 
//' @param x The value at which to calculate the density.
//' @param means A NumericVector of means for the Gaussian distributions.
//' @param std_devs A NumericVector of standard deviations for the Gaussian distributions.
//' @param weights A NumericVector of weights for the Gaussian distributions.
//' @return The computed target density value.
//' @export
// [[Rcpp::export]]
 double targetDensity(double x, const NumericVector& means, 
                      const NumericVector& std_devs, 
                      const NumericVector& weights) {
   double density = 0.0;
   for (int k = 0; k < means.size(); k++) {
     double term = (x - means[k]) / std_devs[k];
     density += weights[k] * std::exp(-0.5 * term * term) / (std_devs[k] * std::sqrt(2 * M_PI));
   }
   return density;
 }

//' @title Generate Multimodal Speckle Pattern using MCMC
//' @description This function generates a multimodal speckle pattern using Markov Chain Monte Carlo (MCMC) methods. 
//' @param rows The number of rows in the output matrix.
//' @param cols The number of columns in the output matrix.
//' @param means A NumericVector of means for the Gaussian distributions.
//' @param std_devs A NumericVector of standard deviations for the Gaussian distributions.
//' @param weights A NumericVector of weights for the Gaussian distributions.
//' @param iterations The number of MCMC iterations.
//' @return A NumericMatrix representing the speckle pattern.
//' @examples
//' \dontrun{
//' rows <- 256
//' cols <- 256
//' means <- c(0.3, 0.7)
//' std_devs <- c(0.1, 0.1)
//' weights <- c(0.5, 0.5)
//' iterations <- 500 
//' speckle <- mcmcCustomMultiModalSpeckle(rows, cols, means, std_devs, weights, iterations)
//' image(speckle, col = gray.colors(256), main = "Custom Multi-Modal Speckle")
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix mcmcCustomMultiModalSpeckle(int rows, int cols, 
                                          NumericVector means, 
                                          NumericVector std_devs, 
                                          NumericVector weights, 
                                          int iterations) {
  if (means.size() != std_devs.size() || means.size() != weights.size()) {
    stop("means, std_devs, and weights must have the same size.");
  }
  
  NumericMatrix speckle(rows, cols);
  
  std::random_device rd;
  std::mt19937 gen(rd());
  std::normal_distribution<> proposal_dist(0, 0.1); // Proposal distribution
  std::uniform_real_distribution<> uniform_dist(0, 1); // Uniform random number
  
  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < cols; j++) {
      double current_sample = uniform_dist(gen); // Initialize sample
      for (int iter = 0; iter < iterations; iter++) {
        // Propose a new sample
        double proposed_sample = current_sample + proposal_dist(gen);
        if (proposed_sample < 0 || proposed_sample > 1) continue; // Reject out-of-bound proposals
        
        // Compute acceptance ratio
        double acceptance_ratio = targetDensity(proposed_sample, means, std_devs, weights) /
          targetDensity(current_sample, means, std_devs, weights);
        if (uniform_dist(gen) < acceptance_ratio) {
          current_sample = proposed_sample;
        }
      }
      speckle(i, j) = current_sample;
    }
  }
  return speckle;
}


