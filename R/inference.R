library(mcmc)
library(nloptr)

#' A function that estimates the user's underlying utility function. 
#' @examples
#'  p <- prefEl(data = data.frame(c(1,0,1), c(0,1,1), c(1,1,1)),
#'              priors = c(Normal(1,0), Exp(0.5), Flat()))
#'  infer(p, estimate = "recommended")
#' @param  p A BayesPrefClass instance
#' @param estimate The type of posterior point-estimate returned. Valid options are "recommended" (default), "MAP", and "mean".
#' @return A vector of parameters that best fits the observed preferences
#' @importFrom nloptr lbfgs
#' @export
infer <- function(p, estimate = "recommended"){
  # Basic escape if data missing
  is.na(p$data) && stop("No data supplied. Populate the ``data'' field with a matrix/dataframe of your alternatives")

  
  # Start by validating our object
  ncol(p$data) == length(p$priors) || stop(paste("Found", length(p$priors), 
                                                 "for", ncol(p$data), "dimensional data. Please supply a prior on each column."))
  # Now that the arguments match, we can set the Sigma field
  p$Sigma <- p$sigma * diag(ncol(p$data))
  
  # Recommended estimate (if applicable). If an exponential distribution exists, 
  # then we generally want the posterior mean. Otherwise, want MAP. 
  if (estimate == "recommended"){
    hasExp <- sum(sapply(p$priors, function(x) "Exp" %in% class(x)))
    if (hasExp){
      estimate <- "mean"
    } else {
      estimate <- "MAP"
    }
  }
  
  # Regardless of method, need MAP estimate. For MCMC, this will be used to start the chain.
  fun <- function(x) -1.0 * .calculateLogProb(x, p) # want to maximize!
  cat(fun(rep(0.0, ncol(p$data))))
  cat("\n")
  map <- lbfgs(rep(0.0, ncol(p$data)), fun)
  if (estimate == "MAP") return(map)
}