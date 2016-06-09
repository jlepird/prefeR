library(mcmc)

#' A function that estimates the user's underlying utility function. 
#' @examples
#'  p <- prefEl(data = data.frame(c(1,0,1), c(0,1,1), c(1,1,1)),
#'              priors = c(Normal(0, 1), Exp(0.5), Flat()))
#'  p$addPref(1 %>% 2)
#'  infer(p, estimate = "recommended")
#' @param  p A BayesPrefClass instance
#' @param estimate The type of posterior point-estimate returned. Valid options are "recommended" (default), "MAP", and "mean".
#' @param nbatch If using Monte Carlo estimates, the number of samples. Defaults to 1000. 
#' @return A vector of parameters that best fits the observed preferences
#' @importFrom stats optim
#' @importFrom mcmc metrop
#' @export
infer <- function(p, estimate = "recommended", nbatch = 1000){
  # Basic escape if data missing
  is.na(p$data) && stop("No data supplied. Populate the ``data'' field with a matrix/dataframe of your alternatives")
  
  # Convert to a matrix object for inference
  p$data <- as.matrix(p$data)

  # keyword validation
  estimate %in% c("recommended",
                  "MAP",
                  "mean",
                  ".RAW_SAMPLES") || # last option used for suggestion algorithm
    stop(paste("Unknown estimate option", estimate))
  
  # Start by validating our object
  ncol(p$data) == length(p$priors) || stop(paste("Found", length(p$priors), 
                                                 "for", ncol(p$data), "dimensional data. Please supply a prior on each column."))
  
  hasFlat <- sum(sapply(p$priors, function(x) "Flat" %in% class(x)))
  hasFlat && length(p$strict) == 0 && stop("Cannot have flat priors and no strict preferences-- see http://futurama.wikia.com/wiki/Neutral.")
  
  # Now that the arguments match, we can set the Sigma field
  p$Sigma <- p$sigma * diag(ncol(p$data))
  
  # Recommended estimate (if applicable). If an exponential distribution exists, 
  # then we generally want the posterior mean. Otherwise, want MAP. 
  if (estimate == "recommended") {
    hasExp <- sum(sapply(p$priors, function(x) "Exp" %in% class(x)))
    if (hasExp) {
      estimate <- "mean"
    } else {
      estimate <- "MAP"
    }
  }
  
  # Regardless of method, need MAP estimate. For MCMC, this will be used to start the chain.
  fun <- function(x) -1.0 * .calculateLogProb(x, p) # want to maximize!
  
  # Need lower/upper bounds in case of exponential priors, otherwise 
  # BFGS will freak out about the corner discontinuity 
  lb <- rep(-Inf, ncol(p$data))
  ub <- rep(Inf,  ncol(p$data))

  # Figure out which priors have a 0 value one one side of 0, and constrain algorithm
  # appropriately 
  lb[is.infinite(sapply(p$priors, function(f) f(-1.0)))] <- 0
  ub[is.infinite(sapply(p$priors, function(f) f( 1.0)))] <- 0
  
  map <- optim(rep(0.0, ncol(p$data)), 
               fun,
               method = "L-BFGS-B",
               lower = lb,
               upper = ub)
  
  # Verify that we converged to our optimmum 
  if (map$convergence != 0) {
    stop(paste("Optimization error", map$message))
  }
  # Peel out the argmax
  map <- map$par
  
  # We might be able to stop here 
  if (estimate == "MAP"){
    names(map) <- colnames(p$data)
    return(map)
  }
  
  # Need to redefine so we get log-prob, not negative log-prob
  fun <- function(x) .calculateLogProb(x, p)
  
  # Run the metropolis hastings algorithm 
  samples <- metrop(fun, map, nbatch = nbatch)
  
  if (estimate == "mean") {
    est <- colMeans(samples$batch)
    names(est) <- colnames(p$data)
    return(est)
  } else {
    return(samples$batch)
  }
}