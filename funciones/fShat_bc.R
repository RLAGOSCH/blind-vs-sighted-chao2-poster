# fShat_bc.R
# ---------------------------------------------------------
# Author: Rodrigo Lagos
# Created: 2023-02-14
# Major update: 2024-08-29
#
# Purpose:
#   Computes the bias-corrected Chao2 richness estimator (Schao2BC)
#   using detection frequencies (vQ), including variance and
#   log-normal confidence intervals.
#
# Input:
#   vQ: A numeric vector where vQ[i] is the number of features mentioned by exactly i participants.
#
# Output:
#   A named list with the following elements:
#     - Shat_BC:      Bias-corrected Chao2 estimate of richness
#     - varShat_BC:   Estimated variance of Shat_BC
#     - Clow_BC:      Lower bound of 95% confidence interval
#     - Cup_BC:       Upper bound of 95% confidence interval

fShat_bc <- function(vQ) {
  # Observed richness: total number of unique features
  S0 <- sum(vQ)
  
  # Total number of participants (based on vector length)
  nT <- length(vQ)
  
  # Correction factor A = (T - 1) / T
  A <- (nT - 1) / nT
  
  # Estimate of undetected features (Q0) with bias correction
  Q0hat_BC <- A * vQ[1] * (vQ[1] - 1) / (2 * (vQ[2] + 1))
  
  # Estimate of total richness
  Shat_BC <- S0 + Q0hat_BC
  
  # Variance estimation
  if (vQ[2] > 0) {
    varShat_BC <- A   * vQ[1]   * (vQ[1] - 1) / (2 * (vQ[2] + 1)) +
      A^2 * vQ[1]   * (2 * vQ[1] - 1)^2 / (4 * (vQ[2] + 1)^2) +
      A^2 * vQ[1]^2 * vQ[2] * (vQ[1] - 1)^2 / (4 * (vQ[2] + 1)^4)
  } else {
    # Fallback variance when Q2 = 0
    varShat_BC <- A   * vQ[1] * (vQ[1] - 1) / 2 +
      A^2 * vQ[1] * (2 * vQ[1] - 1)^2 / 4 -
      A^2 * vQ[1]^4 / (4 * Shat_BC)
  }
  
  # Remove names if present
  varShat_BC <- unname(varShat_BC)
  Shat_BC    <- unname(Shat_BC)
  
  # Confidence Interval using log-normal method (Canessa et al., 2020)
  D <- exp(1.96 * sqrt(log(1 + varShat_BC / (Shat_BC - S0)^2)))
  
  # Compute lower and upper limits
  Clow <- S0 + (Shat_BC - S0) / D
  Cup  <- S0 + (Shat_BC - S0) * D
  
  # Handle degenerate cases (zero variance or zero Q0hat)
  if ((Shat_BC - S0) == 0 || varShat_BC == 0) {
    Clow <- S0
    Cup  <- S0
  }
  
  # Return results as named list
  return(list(
    Shat_BC    = Shat_BC,
    varShat_BC = varShat_BC,
    Clow_BC    = Clow,
    Cup_BC     = Cup
  ))
}
