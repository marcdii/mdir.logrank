#' Two-sample multiple-direction log rank test for stochastic ordered alternatives
#'
#' The mdir.onesided function calculates the multiple-direction logrank
#' statistic for (one-sided) stochastic ordered alternatives and
#' its p-value  based on a wild bootstrap approach
#'
#' @param data A data.frame, list or environment containing the variables \code{time},
#'   \code{event} (with values 0 for censored and 1 for uncensored) and \code{group}.
#' @param group1 The name or the coding for the first group in the data set (neceassary for
#' a one-sided testing problem).
#' @param rg A list containing the exponents \code{c(r, g)} of the directions
#'   \eqn{w(x) = x^r (1-x)^g} or \code{NA}. Both exponents r,g need to be natural numbers including 0.
#'  Default is \code{list(c(0, 0), c(0, 4), c(4, 0))} corresponding to the choice of the
#'  proportional, early and late direction/weight.
#' @param w.user A list containing the user specified functions or \code{NA} (default).
#' @param wild The wild bootstrap approach used for estimating the p-value. The Rademacher
#'   (\code{rade}, default), the normal distribution (\code{norm}) or the centred
#'   Poisson distribution (\code{pois}) approach can be selected.
#' @param iter The number of iteration used for calculating the wild bootstrap p-value.
#'   The default option is 10000.
#' @param dig_p The p-values are rounded to \code{dig_p} digits, the default is 3.
#' @param dig_stat The test statistic is rounded to \code{dig_stat} digits, the default is 3.
#'
#' @details The function provides the multiple-direction logrank statistic for
#'   the two sample one-sided testing problem of stochastic ordering within right-censored survival data.
#'   The null hypothesis \eqn{H:F_1=F_2} is tested against the one-sided alternative \eqn{K:F_1 \ge F_2,
#'   F_1 \neq F_2}. The first group corresponding to \eqn{F_1} can be specified
#'   by the argument \code{group1}. An arbitrary amount of directions/weights of the form
#'   \eqn{w(x) = x^r (1-x)^g} for natural numbers r,g (including 0) can be chosen in the list
#'   \code{rg}. The multiple-direction onesided logrank test needs linearly independent directions.
#'   A check for this is implemented. If the directions chosen by the user are
#'   linearly dependent then a subset consisting of linearly independent directions
#'   is selected automatically. The user can also specify weights of a different shape in the list
#'   \code{w.user}. But if the user specified own weights in \code{w.user} then there is no
#'   automatic check for linear independence.
#'
#'   The \code{mdir.onesided} function returns the test statistic and the p-value
#'   based on a wild bootstrap procedure \code{wild}.
#'
#'
#' @return An  \code{mdirone} object containing the following components:
#' \item{Descriptive}{The directions used and whether the directions specified by the user were
#'    linearly independent.}
#'  \item{p.value}{The p-value of the one-sided multiple-direction logrank test using the
#'    the using the permutation approach (Perm.).}
#'  \item{wild}{The wild bootstrap approach which was used}
#'  \item{stat}{Value of the one-sided multiple-direction logrank statistic.}
#'  \item{rg}{The argument \code{rg}.}
#'  \item{w.user}{The argument \code{w.user}.}
#'  \item{group1}{The name of the first group.}
#'  \item{indep}{logical or NA. \code{indep}\eqn{=}\code{TRUE}/\code{FALSE} when the directions specified by \code{rg} were linearly independent.
#'       \code{indep}\eqn{=}\code{NA} when \code{rg}\eqn{=}\code{NA}.}
#'  \item{iter}{The number of iterations used for calculating the wild bootstrap p-value.}
#' @examples
#' library(coin)
#' data(GTSG)
#' out <- mdir.onesided(data = GTSG, group1 = "Chemotherapy+Radiation", iter = 1000)
#'
#' ## Detailed information:
#' summary(out)
#'
#' @references Ditzhaus, M., Pauly, M. (2018). Wild bootstrap logrank tests with
#' broader power functions for testing superiority. arXiv preprint arXiv:arXiv:1808.05627.
#'
#' @seealso \code{\link[mdir.logrank]{mdir.onesided}}
#'
#' @importFrom stats runif
#' @importFrom utils read.table
#'
#' @export

mdir.onesided <- function(data, group1, rg = list( c(0, 0), c(0, 4), c(4, 0) ), w.user = NA, wild = "rade", iter = 10000, dig_p = 3, dig_stat = 3 ){
  if( sum(c("time","group","event") %in% names(data)) != 3){
    stop("The data does not contain all three variables group, event, time.")
  }

  data_plot <- data.frame( time = data$time, status = data$event, group = data$group)

  # breaking ties
  dist <- runif(length(data$time))*10^(-5)
  data$time <- data$time + dist
  ordering <- order(data$time)

  data <- data.frame( time = data$time[ordering], status = data$event[ordering], group = data$group[ordering])



  data$group <- unlist( lapply( data$group, function(x){ paste0(x)}))
  group1_p <- paste0(group1)
  lev_group <- levels( as.factor(data$group))
  if ( length(lev_group) != 2){
    stop("There are either more or less than 2 groups")
  }
  if ( !(group1_p %in% data$group) ){
    stop("The first group specified by group1 is not a possible group. ")
  }

  group1_lev <- ifelse( group1_p == lev_group[1], 1, 2 )
  data$group <- ifelse( data$group == lev_group[group1_lev], 0, 1)
   # Checking wheter the directions were chosen appropriately
  w <- list()
  rg_na <- FALSE
  if (length(rg) == 1){
    rg_na <- is.na(rg)
  }
  w.user_na <- FALSE
  if (length(w.user) == 1){
    w.user_na <- is.na(w.user)
  }
  if ( rg_na == FALSE){
    rg_un <- unlist( rg )
    if ( all( is.numeric( rg_un )) == FALSE ){
        stop("The exponents r,g contained in rg need to be numeric or rg = NA")
    }
    if ( all( rg_un == floor( rg_un)) == FALSE ){
          stop("The exponents r,g contained in rg need to be natural numbers including 0")
    }
    for (i in 1:length(rg)){
      if (length( rg[[i]] ) != 2){
        stop("rg must be NULL or a list containing vectors of the form c(r, g), where r and g are natural numbers (including 0)!")
      }
      if ( any( rg[[i]] < 0)){
            stop("The exponents specified in the list rg need to be nonnegative!")
        }
    }
    out <- coeff.check(cross = FALSE, rg = rg) # eleminating linearly dependent directions
    indep <- out$indep
    rg <- out$rg
    w.funct <- function( x ){
      x <- unlist(x)
      w <- function(y){ y^x[1] * (1-y)^x[2] }
    }
    match.fun( w.funct )
    w <- lapply( rg, w.funct )
  }else{
    indep <- NA
  }
  if ( (rg_na == TRUE) && (w.user_na == TRUE) ){
    stop("The user have to specify at least one weight by rg or w.user.")
  }
  if (w.user_na == FALSE){
    #Check whether w.user contains functions
    if (any(unlist(lapply(w.user, is.function ))) == FALSE){
      stop("The list w.user contains at least one element which is not a function.")
    }
    w <- c(w,w.user)
  }

  if ( !(wild %in% c("rade", "norm", "pois"))){
    stop("wild is not specified correctly.")
  }

  ## end of direction check --------------------------------------------------------

  n <- length(data$group)
  if ( wild == "rade"){
    out <- teststat_max_cov( data, w, G = 1)
    stat <- out$stat
    stat_boot <- replicate(iter, teststat_max_rade(data = data, w = w,  G = sample( c(-1,1), n, replace = TRUE), Sigma = out$Sigma ))
  }
  if ( wild == "norm"){
    stat <- teststat_max( data, w, G = 1)
    stat_boot <- replicate(iter, teststat_max(data = data, w = w, G = stats::rnorm(n) ))
  }
  if ( wild == "pois"){
    stat <- teststat_max( data, w, G = 1)
    stat_boot <- replicate(iter, teststat_max(data = data, w = w, G = stats::rpois(n,1)-1))
  }
  p_value <- round( mean(  stat_boot > stat ), digits = dig_p)

  # Output --------------------------------------------------------------------------
  dir <- list( rg = rg, indep = indep)
  output <- list( stat = round(stat, digits = dig_stat), wild = wild, group1 = group1, p_value = p_value, rg = rg, w.user = w.user, indep = indep, iter = iter, plotting = data_plot)
  class(output) <- "mdirone"
  return(output)
}

