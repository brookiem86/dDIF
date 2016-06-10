#' Mantel Haenszel DIF Function
#'
#' \code{dmh} identifies DIF using the Mantel Haenszel method
#'
#' @usage dmh(idata, group)
#'
#' @param idata Matrix or data.frame of binary scored item responses.  Should
#'   include only items
#' @param group Numeric vector of values identifying group membership Missing
#'   values are not allowed for group
#'
#' @details
#'
#' The MH method is used to detect uniform DIF.  The equation for the MH
#' statistic was obtained from Holland and Thayer (1988).  The value of
#' 0.5 reflects Yates' correction for continuity and is
#' included in the MH formula.
#'
#' The MH statistic is computed by:
#'
#' \deqn{MH.Stat = [(|\sum[At - \epsilon(At)]| - 0.5)] / [ \sum var(At)]}
#'
#' where
#'
#' \deqn{\epsilon(At) = [nRt*nCt] / nT}
#'
#' and
#'
#' \deqn{var(At) = [nRt*nFt*nCt*nWt] / [nT^2(nT - 1)]}
#'
#' \emph{At}:   number of correct responses for the reference group
#' \emph{Bt}:   number of incorrect responses for the reference group
#' \emph{Ct}:   number of correct responses for the focal group
#' \emph{Dt}:   number of incorrect responses for the focal group
#' \emph{nCt}:  total items correct
#' \emph{nWt}:  total items incorrect
#' \emph{nRt}:  number of individuals in reference group
#' \emph{nFt}:  number of individuals in focal group
#' \emph{nT}:   Total number of responses
#'
#' The common odds ratio is an indication of the degree of association (Mantel
#' & Haenszel, 1959). Values greater than 1 indicate that on average the
#' reference group performed better than the focal group on the item.
#'
#' \deqn{[\sum(At*Dt)/nT] / [\sum(Bt*Ct)/nT]}
#'
#' The common odds ratio is commonly transformed into different scales that
#' are symmetric around 0. ETS's delta scale is a common transformation used
#' in practice in order to better understand the magnitude of DIF
#' (Holland & Thayer, 1985).
#'
#' \deqn{Delta Scale = -2.35[ln(Common Odds Ratio)]}
#'
#' @return A data frame containing the MH statistic, p-values, common odds
#' ratio, and delta scale values for each item
#'
#' @references
#'
#' Hidalgo, M. D. and Lopez-Pina, J.A. (2004). Differential item functioning
#' detection and effect size: a comparison between logistic regression and
#' Mantel-Haenszel procedures. \emph{Educational and Psychological
#' Measurement}, 64, 903-915.
#'
#' Holland, P. W. and Thayer, D. T. (1985). An alternative definition of the
#' ETS delta scale of item difficulty. Research Report RR-85-43. Princeton,
#' NJ: Educational Testing Service.
#'
#' Holland, P. W. and Thayer, D. T. (1988). Differential item performance and
#' the Mantel-Haenszel procedure. In H. Wainer and H. I. Braun (Ed.),
#' \emph{Test validity}. Hillsdale, NJ: Lawrence Erlbaum Associates.
#'
#' Mantel, N. and Haenszel, W. (1959). Statistical aspects of the analysis of
#' data from retrospective studies of disease. \emph{Journal of the National
#' Cancer Institute, 22}, 71 -748.
#'
#' @examples
#'
#' #Loading data
#' data(difdat)
#'
#' # Running dmh
#' dmh(difdat[, 2:21], difdat[, 1])
#'
#' @export
#'


dmh <- function(idata, group) {
  vals <- MH.stat <- c.o.ratio <- delta.scale <- out <-
    ds.mag <- NULL

  for(item in 1:ncol(idata)) {
    exam.tot <- rowSums(idata, na.rm = TRUE)
    scores <- sort(unique(exam.tot))
    exs <- 1:nrow(idata)

    for(a in 1:length(scores)) {
      At <- length(exs[exam.tot == scores[a] & group == 0 &
          idata[, item] == 1])
      Bt <- length(exs[exam.tot == scores[a] & group == 0 &
          idata[, item] == 0])
      Ct <- length(exs[exam.tot == scores[a] & group == 1 &
          idata[, item] == 1])
      Dt <- length(exs[exam.tot == scores[a] & group == 1 &
          idata[, item] == 0])
      nRt <- length(exs[exam.tot == scores[a] & group == 0])
      nFt <- length(exs[exam.tot == scores[a] & group == 1])
      nCt <- length(exs[exam.tot == scores[a] & idata[, item] == 1])
      nWt <- length(exs[exam.tot == scores[a] & idata[, item] == 0])
      nT <- length(exs[exam.tot == scores[a]])

      vals <- rbind(vals, c(At, Bt, Ct, Dt, nRt, nFt, nCt, nWt, nT))
      colnames(vals) <- c("At", "Bt", "Ct", "Dt", "nRt", "nFt", "nCt",
        "nWt", "nT")
    }

    CE.At <- vals[, "nRt"] * vals[, "nCt"] / vals[, "nT"]
    var.At.n <- vals[, "nRt"] * vals[, "nFt"] * vals[, "nCt"] * vals[, "nWt"]
    var.At.d <- ((vals[, "nT"])^2) * ((vals[, "nT"]) - 1)


    MH.stat[item] <- ((abs(sum(vals[, "At"] - CE.At)) -.5)^2) /
      sum(var.At.n / var.At.d)

    c.o.ratio.n <- sum((vals[, "At"] * vals[, "Dt"]) / vals[, "nT"])
    c.o.ratio.d <- sum((vals[, "Bt"] * vals[, "Ct"]) / vals[, "nT"])
    c.o.ratio[[item]] <- c.o.ratio.n / c.o.ratio.d

    delta.scale[item] <- (-2.35) * log(c.o.ratio.n / c.o.ratio.d)

    ds.mag[item] <- mag(delta.scale[item], meth = "ds")
  }

  out <- round(data.frame(MH.stat, c.o.ratio, delta.scale), digits = 4)
  out <- cbind(out, ds.mag)

  return(out)
}
