#' Calculates the log probability of seeing a given set of preferences
#' @param x A guess for our weight vector
#' @param p An object of the Bayes preference class
#' @return A scalar log-likelihood of the guess x
.calculateLogProb <- function(x, p){
  # For each preference stated, get the independent log-probability for it, and sum them 
  # all to get our likelihood. vapply is like sapply but with a default return
  stricts  <- sum(vapply(p$strict, function(pref) .getLogStrictProb(x, pref, p), 0))
  indif    <- sum(vapply(p$indif , function(pref) .getLogIndifProb(x,  pref, p), 0))
  
  # For each prior/guess pair, apply the prior function
  logPrior <- sum(mapply(function(x, prior) prior(x), x, p$priors))

  return(stricts + indif + logPrior)
}

#' Evaluates the likelihood of the observed strict preferences
#' @importFrom stats pnorm
.getLogStrictProb <- function(x, pref, p){
  d <- as.matrix(p$data[pref[[1]], ] - p$data[pref[[2]], ])
  varAlongD <- t(d) %*% p$Sigma %*% d
  meanAlongD <- x %*% d
  return(pnorm(meanAlongD, 0, sqrt(varAlongD), log.p = T)[1])
}

#' Evaluates the likelihood of the observed indifference preferences
#' @importFrom stats pnorm
.getLogIndifProb <- function(x, pref, p){
  d <- as.matrix(p$data[pref[[1]], ] - p$data[pref[[2]], ])
  varAlongD <- t(d) %*% p$Sigma %*% d
  meanAlongD <- x %*% d
  sd <- sqrt(varAlongD)
  return(log(pnorm(meanAlongD + 0.5, 0, sd) - 
             pnorm(meanAlongD - 0.5, 0, sd))[1])
}
