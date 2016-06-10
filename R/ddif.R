#' DIF Evaluation
#'
#' \code{ddif} identifies DIF using MH, LR, or both approaches
#'
#' @usage ddif(idata, group, type)
#'
#' @param idata    a matrix or data.frame of binary scored item responses. Only
#'   includes item data
#' @param group    a numeric vector of values identifying group membership.  The
#'   focal group should be coded as 1 while the reference group should be coded
#'   as 0. Missing values are not allowed for \emph{group}.
#' @param type    identifies the type of method to use.  Options include "mh"
#'  for Mantel Haenszel, "lr" for Logistic Regression, or "all" for both
#'  methods
#'
#' @details
#' The \code{ddif} function calls the \code{\link{dlr}} and/or \code{\link{dmh}}
#' depending on the \emph{type} of method specified.  If the \emph{type} is
#' "all", the results will also display output of DIF magnitude categories for
#' method in a data frame.
#'
#' @seealso
#' \code{\link{dmh}} for MH DIF method
#' \code{\link{dlr}} for LR DIF method
#' \code{\link{R2N}} for Nagelkerke R^2
#' \code{\link{mag}} for magnitude categorization
#'
#' @export

ddif <- function(idata, group, type = "all"){

  mh.res <- lr.res <- all.res <- c.res <- NULL

  if(type == "mh"){
    mh.res <- dmh(idata, group)
    out <- mh.res
  }
  else if(type == "lr"){
    lr.res <- dlr(idata, group)
    out <- lr.res
  }
  else if(type == "all"){
    mh.res <- dmh(idata, group)
    lr.res <- dlr(idata, group)

    mh.cat <- mh.res$ds.mag
    lr.u.cat <- lr.res$UniformDIF[, 4]
    lr.n.cat <- lr.res$NonuniformDIF[, 4]

    dcat <- data.frame(mh.cat, lr.u.cat, lr.n.cat)
    rownames(dcat) <- c(paste("Item", 1:nrow(dcat)))
    colnames(dcat) <- c("MH uDIF", "LR uDIF", "LR nDIF")

    out <- list(mh.res, lr.res, dcat)
    names(out) <- c("MH.Results", "LR.Results", "Magnitude")
  }
  return(out)
  }

