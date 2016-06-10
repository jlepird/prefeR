#' An object containing all data necessary for preference elicitation.
#' @name BayesPrefClass
#' @import methods
#' @field data A matrix or dataframe of data.
#' @field priors A list of functions that give the prior on each variable.
#' @field sigma A scalar value to use for the confusion factor (default 0.1).
#' @field Sigma (Internal use only) A matrix of sigma * diag(ncol(data)).
#' @field strict A list of lists of preferences. For each element x, x[[1]] > x[[2]].
#' @field indif A list of lists of indifferences. For each element x, x[[1]] = x[[2]].
#' @field weights A vector of weights determined by the inference algorithm. 
BayesPrefClass <- setRefClass("BayesPrefClass",
                     fields = c("data",
                                "priors",
                                "sigma",
                                "Sigma", 
                                "strict",
                                "indif",
                                "weights"), 
                     methods = list(
                       initialize = function(...){
                         
                         # Provide default values
                         data    <<- NA
                         priors  <<- list()
                         sigma   <<- 0.1
                         Sigma   <<- NA
                         strict  <<- list()
                         indif   <<- list()
                         weights <<- NA
                         
                         # Call super to override any defaults
                         callSuper(...)
                       }, 
                       show = function(){
                         "Standard printing function"
                         cat("Preference elicitation object with:\n")
                         
                         # Number of observations
                         if (is.null(nrow(data))) {
                           cat("\tNo Data\n")
                         } else {
                           cat(paste0("\t", nrow(data), " observations of ", ncol(data), " variables.\n"))
                         }
                         
                         cat("And the following preferences:\n")
                         # Strict preferences
                         if (length(strict) == 0) {
                           cat("\tNo strict preferences.\n")
                         } else if (length(strict) < 10) {
                           for (x in strict){
                             cat(paste0("\t",x[[1]], " preferred to ", x[[2]], "\n"))
                           } 
                         } else {
                           cat(paste("\t", length(strict), "strict preferences.\n"))
                         }
                         
                         # Indif preferences
                         if (length(indif) == 0) {
                           cat("\tNo indifference preferences.\n")
                         } else if (length(indif) < 10) {
                           for (x in indif) {
                             cat(paste0("\t", x[[1]], " indifferent to ", x[[2]], "\n"))
                           } 
                         } else {
                           cat(paste("\t", length(indif), "indifference preferences.\n"))
                         }
                       },
                       addPref = function(x) {
                         "Adds a preference created using \\%>\\%, \\%<\\%, or \\%=\\%."
                         
                         # Make sure new rownames exist
                         lapply(x, function(a){
                           a %in% row.names(data) || (is.numeric(a) && a <= nrow(data)) || 
                             stop(paste("Row", a, "not found in data."))
                         })
                         
                         # Undo any previous inference
                         weights <<- NA
                         
                         if ("strict" %in% class(x)) {
                           strict <<- append(strict, list(x))
                           return()
                         } else if ("indif"  %in% class(x)) {
                           indif <<- append(indif,  list(x))
                           return()
                         }
                         stop("Unknown input type. \nPlease create preferences using %>%, %<%, or %=%.")
                       },
                       infer = function(estimate = "recommended") {
                         "Calls the ``infer'' function to guess weights" 
                         weights <<- BayesPref::infer(.self, estimate = estimate) # have to be careful with namespace here
                         return(weights)
                         },
                       suggest = function(maxComparisons = 10) {
                         "Calls the ``suggest'' function to guess weights" 
                         BayesPref::suggest(.self, maxComparisons = maxComparisons) # have to be careful with namespace here
                       },
                       rank = function(){
                         "Calculates the utilty of each row in our dataset"
                         is.na(data) && stop("No data supplied")
                         
                         # Calculate weights if we don't already have them
                         is.na(weights) && infer()
                         utilities <- apply(data, 1, # over rows
                                            function(x){
                                              weights %*% x
                                            })
                         names(utilities) <- row.names(data)
                         utilities <- sort(utilities, decreasing = T)
                         return(utilities)
                       }
                     )
)


#' A shortcut to create objects of the class BayesPrefClass
#' @examples 
#' p <- prefEl(data = data.frame(x = c(1,0,1), y = c(0, 1, 1)),
#'             priors = c(Normal(0,1), Flat()))
#' help(BayesPrefClass)
#' @param data A matrix or dataframe of data. Each column should be a variable, each row an observation.
#' @param priors A list of functions that give the prior on each variable. E.g. see help(Flat)
#' @param  ... Other parameters to pass to the class constructor. Not recommended. 
#' @export
prefEl <- function(data = NA, priors = list(), ...) BayesPrefClass(data = data, priors = priors, ...)



# Tools to add in preferences 

#' A helper function to add in preferences in a user-friendly way 
#' @examples 1 %>% 2 # prefer row 1 to row 2
#' @param  a The preferred row
#' @param  b The nonpreferred row
#' @export
#' @family preferences
`%>%` <- function(a,b){
  # Need to check if function existed already, i.e. in dplyr
  ret <- NULL
if (existsFunction("%>%")) {
  oldGt <- `%>%`
    if(class(a) == class(b) & class(a) %in% c("numeric", "character")){
      ret <- list(a,b)
    } else {
      return(oldGt(a,b))
    }
} else {
  ret <- list(a,b)
}
  class(ret) <- c("list", "strict")
  return(ret)
}

#' A helper function to add in preferences in a user-friendly way 
#' @examples 1 %<% 2 # prefer row 2 to row 1
#' @param  b The preferred row
#' @param  a The nonpreferred row
#' @family preferences
#' @export
`%<%` <- function(a,b){
  ret <- NULL
  # Need to check if function existed already, i.e. in dplyr
  if (existsFunction("%<%")) {
    oldLt <- `%<%`
    if (class(a) == class(b) & class(a) %in% c("numeric", "character")){
      ret <- list(b, a)
    } else {
      return(oldLt(a, b))
    }
  } else {
    ret <- list(b, a)
  }
  class(ret) <- c("list", "strict")
  return(ret)
}


#' A helper function to add in preferences in a user-friendly way 
#' @examples 1 %<% 2 # prefer row 2 to row 1
#' @param  b The preferred row
#' @param  a The nonpreferred row
#' @family preferences
#' @export
`%=%` <- function(a,b){
  ret <- NULL
  # Need to check if function existed already, i.e. in dplyr
  if (existsFunction("%=%")) {
    oldEq <- `%=%`
    if (class(a) == class(b) & class(a) %in% c("numeric", "character")){
      ret <- (list(a, b))
    } else {
      return(oldEq(a, b))
    }
  } else {
    ret <- (list(a, b))
  }
  class(ret) <- c("list", "indif")
  return(ret)
}

#' A convinience function for generating Normal priors
#' @examples Normal(0, 1)(1) == dnorm(1, log = TRUE)
#' @param  mu The mean of the normal distribution
#' @param  sigma The standard deviation of the prior
#' @family priors
#' @importFrom stats dnorm
#' @export
#' @return A function yielding the log-PDF at x of a normal distribution with given statistics.
Normal <- function(mu = 0.0, sigma = 1.0){
  f <- function(x) dnorm(x, mu, sigma, log = T)
  class(f) <- c("function", "prior", "Normal")
  return(f)
}

#' A convinience function for generating Exponential priors
#' @examples Exp(1)(1) == dexp(1,1, log = TRUE)
#' @param  mu The mean of the exponential distribution, i.e. \eqn{1/rate}
#' @family priors
#' @importFrom stats dexp
#' @export
#' @return A function yielding the log-PDF at x of a exponential distribution with given statistics.
Exp <- function(mu = 1.0){
  f <- function(x) dexp(sign(mu) * x, sign(mu) * 1.0 / mu, log = T) # need to use sign to allow for negative means
  class(f) <- c("function", "prior", "Exp")
  return(f)
}

#' A convinience function for generating a flat prior
#' @examples Flat()(1) == 0.0
#' @family priors
#' @export
#' @return The zero function.
Flat <- function(){
  f <- function(x) 0.0
  class(f) <- c("function", "prior", "Flat")
  return(f)
}
