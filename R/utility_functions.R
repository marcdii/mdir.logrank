#' @export
print.mdirLR <- function(x, ...) {
  cat("test statistic: ", "\n", "     ", x$stat, sep = "")
  df <- length(x$rg)+ sum(x$cross)
  cat("\n", "p-value (chi-squared Approx. with ",
      df, " df): ",
      "\n", "     ", x$p_value$Approx, sep = "")
  cat("\n", "p-value (", x$nperm, " permutations): ",
      "\n", "     ", x$p_value$Perm, sep = "")
}

#' @export
summary.mdirLR <- function (object, ...) {
  x <- object
  if ( length(x$rg) == 0 ){
    cat("The chosen weights are linearly independent.", "\n",
        "The test is based on the crossing weight.", "\n","\n")
  }else{
        rg_rep <- paste0("(", x$rg[[1]][1], ",", x$rg[[1]][2], ")" )
        if ( length(x$rg) > 1 ){
          for (i in 2:length(x$rg)){
            rg_rep <- paste0( rg_rep, ", (", x$rg[[i]][1], ",", x$rg[[i]][2], ")" )
          }
        }
        cat("The chosen weights are", if ( x$indep == FALSE){ " not"},
            " linearly independent.", "\n", "The test is based on ",
            if ( x$cross == TRUE){ "the crossing weight and "},
            length(x$rg), " ","weight", if (length(x$rg) > 1){"s"}, " with exponents ", "\n",
            "     ", "(r,g) =  ", rg_rep, ".", "\n","\n", sep="")
  }
  print(x)
}

#' @export
print.mdirone <- function(x, ...) {
  cat("test statistic: ", "\n", "     ", x$stat, sep = "")
  if ( x$wild == "rade" ){
    boot <- "Rademacher"
  }
  if ( x$wild == "norm" ){
    boot <- "normal"
  }
  if ( x$wild == "pois" ){
    boot <- "centered Poisson"
  }
  cat("\n", "p-value (", boot, " wild bootstrap, ", x$iter, " iterations): ",
      "\n", "     ", x$p_value, sep = "")
}

#' @export
summary.mdirone <- function (object, ...) {
   x <- object
   w.user <- x$w.user
   w.user_na <- FALSE
   if (length(w.user) == 1){
     w.user_na <- is.na(w.user)
   }
   if (is.na( x$indep) == TRUE){
     cat("The test is only based on weights specified by w.user.", "\n", if (is.numeric(x$group1) == TRUE){ paste("The first group is coded by", x$group1)}else{
           paste("The first group is named as", x$group1)
         }
         , ".", "\n", "\n", sep="")
     print(x)
   }else{
    rg_rep <- paste0("(", x$rg[[1]][1],",", x$rg[[1]][2], ")" )
    if ( length(x$rg) > 1 ){
      for (i in 2:length(x$rg)){
        rg_rep <- paste0( rg_rep, ", (", x$rg[[i]][1],",", x$rg[[i]][2], ")" )
      }
    }
    cat("The weights chosen by rg are", if ( x$indep == FALSE){ " not"},
        " linearly independent.", "\n", "The test is based on ",
        length(x$rg), " ","weight", if (length(x$rg) > 1){"s"}, " with exponents ", "\n",
        "     ", "(r,g) =  ", rg_rep, if (w.user_na == TRUE){ "."}else{"\n"},if (w.user_na == FALSE){"and weights specified by w.user."}, "\n","\n",
        if (is.numeric(x$group1) == TRUE){ paste("The first group is coded by", x$group1)}else{
          paste("The first group is named as", x$group1)
        }
          , ".", "\n", "\n", sep="")
  print(x)
   }
}

#' @export
plot.mdirone <- function (x,...) {
  plotting <- x$plotting
  requireNamespace("survival", quietly = TRUE)

  if (!("package:survival" %in% search())) {
    attachNamespace("survival")
  }
  requireNamespace("survminer", quietly = TRUE)

  if (!("package:survminer" %in% search())) {
    attachNamespace("survminer")
  }


  fit <- survival::survfit(Surv(time,status) ~ group, data = plotting)

  plot_1 <- survminer::ggsurvplot(fit, data = plotting, fun = "pct",
                                  censor = FALSE,
                                  #ggtheme = theme_bw(),
                                  pval = FALSE)
  return(plot_1)

}
#' @export
plot.mdirLR <- function (x,...) {
  plotting <- x$plotting
  requireNamespace("survival", quietly = TRUE)

  if (!("package:survival" %in% search())) {
    attachNamespace("survival")
  }
  requireNamespace("survminer", quietly = TRUE)

  if (!("package:survminer" %in% search())) {
    attachNamespace("survminer")
  }


  fit <- survival::survfit(Surv(time,status) ~ group, data = plotting)

  plot_1 <- survminer::ggsurvplot(fit, data = plotting, fun = "pct",
                                  censor = FALSE,
                                  #ggtheme = theme_bw(),
                                  pval = FALSE)
  return(plot_1)

}
