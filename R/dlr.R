#' Logistic Regression DIF Function
#'
#' \code{dlr} identifies DIF using the logistic regression statistic method
#'
#' @usage dlr(idata, group, alpha = 0.05)
#'
#' @param idata    a matrix or data.frame of binary scored item responses. Only
#'   includes item data
#' @param group    a numeric vector of values identifying group membership.  The
#'   focal group should be coded as 1 while the reference group should be coded
#'   as 0. Missing values are not allowed for \emph{group}.
#' @param alpha    identifies the alpha value used in the analyses. The default
#'   value is 0.05.
#'
#' @details
#' The dlr function computes logistic regression results within a DIF
#' framework specified by Swaminathan and Rogers (1990).  Three models are
#' traditionally fitted in order to determine if an item has mixed,
#' nonuniform, or uniformed DIF.  The three models that are fitted include:
#'
#' \deqn{M0: logit(\pig) = \beta0 + \beta1\theta + \beta2g + \beta3\thetag}
#' \deqn{M1: logit(\pig) = \beta0 + \beta1\theta + \beta2g}
#' \deqn{M2: logit(\pig) = \beta0 + \beta1\theta}
#'
#' where
#' \eqn{\pig} is the probability of correctly answering an item in
#' group \emph{g},
#' \eqn{\theta} is examinee’s matching criteria, and
#' \emph{g} is the group membership of the examinee.
#'
#' Nonuniform DIF is assessed by comparing the \emph{M0} and \emph{M1} models.
#' Uniform DIF is evaluated by comparing the \emph{M1} and \emph{M2} models.
#'
#' The magnitude of DIF is calculated by subtracting the \emph{R}^2 estimate
#' of the compact model from the \emph{R}^2 value of the augmented model.
#' Nagelkerke  R^2 estimate (Nagelkerke, 1991) is used to compute the DIF
#' effect size. Classification indices of the magnitude of DIF are based on
#' values suggested by Zumbo and Thomas (1997).  classifications of \emph{A},
#' \emph{B}, and \emph{C} represent negligible, moderate, and large DIF effect
#' sizes.
#'
#' \bold{A}:  delta{R^2} < or = .13
#' \bold{B}:  .13 < delta{R^2} < or = .26
#' \bold{C}:  delta{R^2} > .26
#'
#' @return A list containing:
#'
#' LRModels        a list containing three data frames consisting of the
#'                 results from the M0 M1 and M2 LR models
#'
#' UniformDIF      a data frame consisting of the uniform DIF results
#'
#' NonuniformDIF   a data frame consisting of the uniform DIF results
#'
#' @references
#'
#' Hidalgo, M. D. and Lopez-Pina, J.A. (2004). Differential item functioning
#' detection and effect size: a comparison between logistic regression and
#' Mantel-Haenszel procedures. \emph{Educational and Psychological
#' Measurement}, 64, 903-915.
#'
#' Nagelkerke, N. J. D. (1991). A note on a general definition of the
#' coefficient of determination. \emph{Biometrika}, 78, 691-692.
#'
#' Swaminathan, H. and Rogers, H. J. (1990). Detecting differential item
#' functioning using logistic regression procedures. \emph{Journal of
#' Educational Measurement}, 27, 361-370.
#'
#' Zumbo, B. D. (1999). \emph{A handbook on the theory and methods of
#' differential item functioning (DIF): Logistic regression modeling as a
#' unitary framework for binary and Likert-type (ordinal) item scores}. Ottawa,
#' Canada: Directorate of Human Resources Research and Evaluation, Department
#' of National Defense.
#'
#' Zumbo, B. D.,&Thomas, D. R. (1997). \emph{A measure of effect size for a
#' model-based approach for studying DIF} (Working paper of the Edgeworth
#' Laboratory for Quantitative Behavioral Science). Prince George, Canada:
#' University of Northern British Columbia.
#'
#' @seealso \code{\link{R2N}} for Nagelkerke R^2
#'
#' @examples
#' #Loading data
#' data(difdat)
#'
#' #Testing DIF using LR approach alpha = .05
#' dif.lr <- dlr(difdat[, 1:21], difdat[, 1])
#'
#' #Testing DIF using LR approach alpha = .01
#' dif.lr <- dlr(difdat[, 1:21], difdat[, 1], alpha = .01)
#'
#' @export

dlr <- function(idata, group, alpha = .05){

  nu.stat <- u.stat <- nu.pval <- u.pval <- nu.R2N <- u.R2N <- nu.mag <-
    u.mag <- m0R2N <- m1R2N <- m2R2N <- NULL

  m0co <- matrix(0, ncol(idata), 4)
  m1co <- matrix(0, ncol(idata), 3)
  m2co <- matrix(0, ncol(idata), 2)

  for(i in 1:ncol(idata)) {

    exam.tot <- rowSums(idata, na.rm = TRUE)
    itemp <- idata[, i]

    m0 <- glm(itemp ~ exam.tot * group, family = "binomial")
    m1 <- glm(itemp ~ exam.tot + group, family = "binomial")
    m2 <- glm(itemp ~ exam.tot, family = "binomial")

    m0co[i, 1:length(m0$coefficients)] <- m0$coefficients
    m1co[i, 1:length(m1$coefficients)] <- m1$coefficients
    m2co[i, 1:length(m2$coefficients)] <- m2$coefficients

    nu.stat[i] <- deviance(m1) - deviance(m0)
    u.stat[i] <- deviance(m2) - deviance(m1)

    nu.pval[i] <- pchisq(nu.stat[i], 1, lower.tail = FALSE)
    u.pval[i] <- pchisq(u.stat[i], 1, lower.tail = FALSE)

    m0R2N[i] <- R2N(m0, nrow(idata))
    m1R2N[i] <- R2N(m1, nrow(idata))
    m2R2N[i] <- R2N(m2, nrow(idata))

    nu.R2N[i] <- m0R2N[i] - m1R2N[i]
    u.R2N[i] <- m1R2N[i] - m2R2N[i]

    nu.mag[i] <- mag(nu.R2N[i], meth = "zt")
    u.mag[i] <- mag(nu.R2N[i], meth = "zt")
  }
  model0 <- round(cbind(m0co, m0R2N), digits = 4)
  model1 <- round(cbind(m1co, m1R2N), digits = 4)
  model2 <- round(cbind(m2co, m2R2N), digits = 4)
  colnames(model0) <- c("Intercept", "Score", "Group", "Score*Group", "R^2")
  colnames(model1) <- c("Intercept", "Score", "Group", "R^2")
  colnames(model2) <- c("Intercept", "Score", "R^2")
  rownames(model0) <- rownames(model1) <- rownames(model2) <-
    c(paste("Item", 1:nrow(model0)))

  chi.crit <- qchisq(1 - alpha, 1)

  LRmodels <- list(model0, model1, model2)
  names(LRmodels) <- c("M0", "M1", "M2")

  A <- "Negligible"
  B <- "Moderate"
  C <- "Large"
  ztmag <- rbind(A, B, C)
  colnames(ztmag) <- "Magnitude"

  as.numeric(c(nu.stat, nu.pval, nu.R2N, u.stat, u.pval, u.R2N))

  nu.dif <- data.frame(nu.stat, nu.pval, nu.R2N)
  nu.dif <- round(nu.dif, digits = 4)
  nu.dif <- cbind.data.frame(nu.dif, nu.mag)

  u.dif <- data.frame(u.stat, u.pval, u.R2N)
  u.dif <- round(u.dif, digits = 4)
  u.dif <- data.frame(u.dif, u.mag)

  colnames(nu.dif) <- colnames(u.dif) <- c("Stat", "P-Value", "delta.R2",
    "ZT")
  rownames(nu.dif) <- rownames(u.dif) <- c(paste("Item", 1:nrow(model0)))

  LRout <- list(LRmodels, ztmag, u.dif, nu.dif)
  names(LRout) <- c("LRModels", "ZT", "UniformDIF", "NonuniformDIF")
  return(LRout)
}
