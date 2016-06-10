#' Nagelkerke R^2
#'
#' \code{R2N} produces Nagelkerke’s \emph{R^2} estimate for the specified LR
#' model.  Used in the function \code{\link{LRd}}
#'
#' @usage R2N(mod, n)
#'
#' @param n the total sample size
#'
#' @param mod an object of class glm
#'
#' @details
#' The magnitude of DIF is calculated by subtracting the \emph{R^2} estimate
#' of the compact model from the \emph{R^2} value of the augmented model.
#' Several pseudo R^2 estimates are available.  The Nagelkerke ?\emph{R^2}
#' estimate (Nagelkerke, 1991) is used to compute the DIF effect size for the
#' \code{LRd} function.
#'
#' Nagelkerke R^2 is estimated using the equations below:
#'
#' \deqn{R^2_CS = 1 - (exp(- LL(M_null)/2 + LL(M_obs)/2))^(2/n)}
#' \deqn{R^2_MAX = 1 - (exp(-LL(M_null)/2))^(2/n)}
#' \deqn{R^2_CS/R^2_MAX}
#'
#' @return a numeric value estimating Nagelkerke R^2
#'
#' @references
#'
#' Nagelkerke, N. J. D. (1991). A note on a general definition of the
#' coefficient of determination. \emph{Biometrika}, 78, 691-692.
#'
#' @seealso \code{\link{LRd}} for LR DIF
#'
#' @export
#'
R2N <- function(mod, n){
  R2cs <- 1 - (exp(-mod$null.deviance/2 + mod$deviance/2))^(2/n)
  R2max <- 1 - (exp(-mod$null.deviance/2))^(2/n)
  R2Nout <- R2cs/R2max
  return(R2Nout)
}
