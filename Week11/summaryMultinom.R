summaryMultinom <- 
function (object) 
{
  library(car)
  M <- summary(object, Wald = TRUE)
  cat("Call:\n")
  print(M$call)
  if (terms(object)[[3]] != 1) {
    A <- Anova(object)
  }
  N <- dim(M$Wald.ratios)
  n <- prod(N)
  lH <- numeric(n)
  zeros <- rep(0, n)
  if (!is.null(N) && n > 1) {
    for (i in 1:n) {
      z.tmp <- zeros
      z.tmp[i] <- 1
      lH[i] <- linearHypothesis(object, z.tmp, rhs = 0)[2, 
                                                        3]
    }
    p.values <- matrix(lH, N[1], N[2], byrow = TRUE)
    result <- cbind(as.vector(t(M$coefficients)), as.vector(t(M$standard.errors)), 
                    as.vector(t(M$Wald.ratios)), as.vector(t(p.values)))
    colnames(result) <- c("Coef", "SE Coef", "Z", "P")
  }
  else {
    result <- cbind(as.vector(t(M$coefficients)), as.vector(t(M$standard.errors)), 
                    as.vector(t(M$Wald.ratios)))
    colnames(result) <- c("Coef", "SE Coef", "Z")
  }
  rn <- rownames(M$coefficients)
  cn <- colnames(M$coefficients)
  j <- 0
  if (!is.null(N) && n > 1) {
    for (i in 1:N[1]) {
      R <- result[1:N[2] + j, , drop = FALSE]
      dimnames(R) <- eval(parse(text = paste("list('", 
                                             rn[i], "'=c('", paste(cn, collapse = "','", sep = ""), 
                                             "'),", "c('Coef','SE Coef','Z','P'))", sep = "")))
      j <- j + N[2]
      print(R)
    }
  }
  cat("\n")
  if (terms(object)[[3]] != 1) {
    print(A)
  }
  cat(paste("\nResidual Deviance: ", round(M$deviance, 4), 
            "\n", sep = ""))
  cat(paste("AIC: ", round(M$AIC, 4), "\n", sep = ""))
  print(logLik(object))
}