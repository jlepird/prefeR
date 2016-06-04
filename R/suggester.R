#' Suggests a good comparision for the user to make next
#' @export
#' @param p An object of class BayesPrefClass.
#' @return A two-element vector of recommended comparisons.
suggest <- function(p){
  # Get (or make up) row names
  rn <- row.names(p$data)
  if (is.null(rn)) rn <- c(1:nrow(p$data))
  
  # Dataframe of possibilities 
  poss <- expand.grid(rn, rn)
  poss <- subset(poss, poss$Var1 > poss$Var2) # remove reversed duplicates and self-comparisons
  # Calculate our current best guess
  bestGuess <- infer(p)
  
  # Calculate the expected posterior entropy of each possible comparison
  poss$entropy <- mapply(function(x,y) .estimateEntropy(p, bestGuess, x, y), poss$Var1, poss$Var2)
  
  id <- which.min(poss$entropy)
  return(c(poss$Var1[id], poss$Var2[id]))
}

#' Calculates the expected posterior entropy of the prefel object if x and y are compared
#' @param x Possible comparison 1
#' @param y Possible comparison 2
#' @param p An object of class BayesPrefClass.
#' @param currentGuess The current best estimate for our weight vector. 
.estimateEntropy <- function(p, currentGuess, x, y){
  # Calculate prob x > y under current guess
  pXgtY <- exp(.getLogStrictProb(currentGuess, list(x,y), p))
  originalStrict <- p$strict
  p$strict <- append(originalStrict, list(list(x,y)))
  sampsXgtY <- infer(p, estimate = ".RAW_SAMPLES")
  entXgtY   <- .sampleEntropy(sampsXgtY)
  p$strict <- append(originalStrict, list(list(y,x)))
  sampsYgtX <- infer(p, estimate = ".RAW_SAMPLES")
  entYgtX   <- .sampleEntropy(sampsYgtX)
  p$strict <- originalStrict
  return(pXgtY * entXgtY + (1.0 - pXgtY) * entYgtX)
}

#' Calculates the entropy of a matrix of samples.
#' @param X a matrix where each row is a sample of variables in different columns
#' @importFrom stats density
.sampleEntropy <- function(X){
  # Assume independence across columns
  kdes <- apply(X, 2, function(x){
    k <- density(x)
    return(-sum(k$y * log(k$y)))
  })
  return(sum(kdes))
}