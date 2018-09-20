#' Compute bias as a percentage
#'
#' @param obs Observed values as a numeric vector.
#' @param sim Simulated values values as a numeric vector.
#'
#' @return If successful returns the percentage of bias as an integer. If unsuccessful, returns the value \code{FALSE}.
#' @author Muluneh A. Mekonnen
#' @export
#'
#' @examples
#' obs <- runif(100)
#' sim <- runif(100)
#' PBIAS(obs, sim)
PBIAS <- function(obs, sim) {
  obs <- na.omit(obs)
  sim <- na.omit(sim)
  if (length(obs) != length(sim)) {
    cat("Error: observed and simulated values are different lengths\n")
    return(FALSE)
  }
    
  PB <- round(sum(obs - sim) * 100 / sum(obs), 0)
  return(PB)
}
