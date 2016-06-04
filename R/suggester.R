#' Suggests a good comparision for the user to make next. 
#' @export
#' @examples 
#' p <- prefEl()
#' @param p An object of class BayesPrefClass.
#' @return A two-element vector of recommended comparisons.
suggest <- function(p){
  
  # Convert to a matrix object for inference (if not already)
  p$data <- as.matrix(p$data)
  
  # Get (or make up) row names
  rn <- row.names(p$data)
  if (is.null(rn)) rn <- c(1:nrow(p$data))
  
  # Dataframe of possibilities 
  poss <- expand.grid(rn, rn, stringsAsFactors = F)
  
  # Figure out all the ones we already asked 
  stated <- append(p$strict, p$indif)
  poss$redundent <- 
    mapply(function(a, b){
      max(sapply(stated, function(x){
       (x[[1]] == a && x[[2]] == b) || (x[[2]] == a && x[[1]] == b)
      }))
    }, 
    poss$Var1, poss$Var2)
  
  poss <- subset(poss, !poss$redundent)       # remove ones we already asked 
  poss <- subset(poss, poss$Var1 > poss$Var2) # remove reversed duplicates and self-comparisons
  
  nrow(poss) == 0 && stop("All possible comparisons have already been made!")
  
  # Calculate our current best guess
  bestGuess <- infer(p)
  
  # Calculate the expected posterior entropy of each possible comparison
  poss$entropy <- mapply(function(x,y) .estimateEntropy(p, bestGuess, x, y), poss$Var1, poss$Var2)
  
  id <- which.min(poss$entropy)
  return(c(poss$Var1[id], poss$Var2[id]))
}

#' Calculates the expected posterior entropy of the prefel object if x and y are compared. Ignores 
#' the odds of indifference preferences, as using them would increase runtime 50% without much gain.
#' @param x Possible comparison 1
#' @param y Possible comparison 2
#' @param p An object of class BayesPrefClass.
#' @param currentGuess The current best estimate for our weight vector. 
.estimateEntropy <- function(p, currentGuess, x, y){
  
  # Calculate prob x > y under current guess
  pXgtY <- exp(.getLogStrictProb(currentGuess, list(x,y), p))
  
  # Record original strict values
  originalStrict <- p$strict
  
  # Add in that x > y
  p$strict <- append(originalStrict, list(list(x,y)))
  
  # Get the raw samples for this new posterior, and estimate its entropy
  sampsXgtY <- infer(p, estimate = ".RAW_SAMPLES")
  entXgtY   <- .sampleEntropy(sampsXgtY)
  
  # Do the same as above, but with x > y 
  p$strict <- append(originalStrict, list(list(y,x)))
  sampsYgtX <- infer(p, estimate = ".RAW_SAMPLES")
  entYgtX   <- .sampleEntropy(sampsYgtX)
  p$strict <- originalStrict
  
  # Return expected posterior entropy
  return(pXgtY * entXgtY + (1.0 - pXgtY) * entYgtX)
}

#' Calculates the entropy of a matrix of samples.
#' @param X a matrix where each row is a sample of variables in different columns
#' @importFrom entropy entropy
#' @importFrom entropy discretize
.sampleEntropy <- function(X){
  # Assume independence across columns-- totally wrong, but necessary to maintain computational tractability 
  kdes <- apply(X, 2, function(x){
    return(entropy(discretize(x, numBins = length(x)/10)))
  })
  return(sum(kdes))
}