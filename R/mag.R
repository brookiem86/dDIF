#' DIF Magnitude Calculation
#'
#' \code{mag} categorizes the DIF magnitudes
#'
#' @usage mag(m, meth)
#'
#' @param m value used for calculating DIF magnitudes
#'
#' @param meth type of method to calculate. Options include "zt" for
#' logistic regression magnitude values suggested by Zumbo and Thomas
#' (1997), and "ds" for the MH magnitude value categories from using
#' ETS's delta scale (Holland & Thayer, 1985).
#'
#' This function is used to categorize DIF magnitudes in the functions
#' \code{\link{LRd}} and \code{\link{MHd}}
#'
#' @return a character value representing the DIF magnitude categorization
#'
#' @references
#'
#' Holland, P. W. and Thayer, D. T. (1985). An alternative definition of the
#' ETS delta scale of item difficulty. Research Report RR-85-43. Princeton,
#' NJ: Educational Testing Service.
#'
#' Zumbo, B. D.,&Thomas, D. R. (1997). \emph{A measure of effect size for a
#' model-based approach for studying DIF} (Working paper of the Edgeworth
#' Laboratory for Quantitative Behavioral Science). Prince George, Canada:
#' University of Northern British Columbia.
#'
#' @seealso \code{\link{LRd}} for LR DIF method, \code{\link{MHd}} for MH DIF method
#'
#' @export
#'

mag <- function(m, meth){
  m.out <- NULL

  m.zt <- function(m){
    if(m <= .13){
      m.out <- "A"
    }
    else if(m > .13 & m <= .26){
      m.out <- "B"
    }
    else {
      m.out <- "C"
    }
    return(m.out)
  }

  m.ets <- function(m){
    if(m <= 1){
      m.out <- "A"
    }
    else if(m > 1 & m <= 1.5){
      m.out <- "B"
    }
    else {
      m.out <- "C"
    }
    return(m.out)
  }

  if (meth == "zt"){
    m.zt(m)
  }
  else {if(meth == "ds"){
    m.ets(m)
  }
  }
}
