# fGetQ.R
# ---------------------------------------------------------
# Author: Rodrigo Lagos
# Created: 2024-06-27
#
# Purpose:
#   Computes the vector Q from an incidence matrix (mtI),
#   where Q[i] is the number of features mentioned by exactly i participants.
#
# Input:
#   mtI: A binary incidence matrix (features × participants), where
#        rows are features and columns are participants. Each cell contains
#        1 if the participant listed the feature, 0 otherwise.
#
# Output:
#   A named vector vQ of counts, where:
#     - vQ[i] = number of features mentioned exactly i times
#     - names(vQ) = "Q_1", "Q_2", ..., "Q_T"

fGetQ_V2 <- function(nT
                     , vFq) {


  # Initialize Q vector of length T (number of participants)
  vQ <- (1:nT) * 0
  
  # Count how many features occur exactly i times
  # and assign to the appropriate position in vQ
  vQ[as.numeric(names(table(vFq)))] <- table(vFq)
  
  # Add names: Q_1, Q_2, ..., Q_nT
  names(vQ) <- paste0("Q_", 1:nT)
  
  return(vQ)
}
