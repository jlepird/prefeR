#' An object containing all data necessary for preference elicitation.
#' @name BayesPrefClass
#' @import methods
#' @exportClass BayesPrefClass
#' @field data A matrix or dataframe of data.
#' @field priors A list of functions that give the prior on each variable.
#' @field sigma A scalar value to use for the confusion factor (default 0.1).
#' @field strict A list of lists of preferences. For each element x, x[[1]] > x[[2]].
#' @field indif A list of lists of indifferences. For each element x, x[[1]] = x[[2]].
BayesPrefClass <- setRefClass("BayesPrefClass",
                     fields = c("data",
                                "priors",
                                "sigma",
                                "strict",
                                "indif"), 
                     methods = list(
                       initialize = function(...){
                         
                         # Provide default values
                         data   <<- NA
                         priors <<- list()
                         sigma  <<- 0.1
                         strict <<- list()
                         indif  <<- list()
                         # Call super to override any defaults
                         callSuper(...)
                       }, 
                       show = function(){
                         "Standard printing function"
                         cat("Preference elicitation object with:\n")
                         
                         # Number of observations
                         if (is.null(nrow(data))){
                           cat("\tNo Data\n")
                         } else {
                           cat(paste("\t", nrow(data), "observations of", ncol(data), "variables.\n"))
                         }
                         
                         cat("And the following preferences:\n")
                         # Strict preferences
                         if (length(strict) == 0){
                           cat("\tNo strict preferences.\n")
                         } else if (length(strict) < 10){
                           for (x in strict){
                             cat(paste0("\t",x[[1]], " preferred to ", x[[2]], "\n"))
                           } 
                         } else {
                           cat(paste("\t", length(strict), "strict preferences.\n"))
                         }
                         
                         # Indif preferences
                         if (length(indif) == 0){
                           cat("\tNo indifference preferences.\n")
                         } else if (length(indif) < 10){
                           for (x in indif){
                             cat(paste0("\t", x[1], " indifferent to ", x[2], "\n"))
                           } 
                         } else {
                           cat(paste("\t", length(indif), "indifference preferences.\n"))
                         }
                       },
                       addPref = function(x){
                         "Adds a preference created using \\%>\\%, \\%<\\%, or \\%=\\%."
                         if ("strict" %in% class(x)) strict <<- append(strict, list(x));
                         if ("indif" %in% class(x))  indif  <<- append(indif, list(x)); return()
                         stop("Unknown input type. \nPlease create preferences using %>%, %<%, or %=%.")
                       }
                     )
)


#' A shortcut to create objects of the class BayesPrefClass
#' @examples help(BayesPrefClass)
#' @param  ... Arguments to pass on to BayesPrefClass constructor
#' @export
prefEl <- function(...) BayesPrefClass(...)



# Tools to add in preferences 

#' A helper function to add in preferences in a user-friendly way 
#' @examples 1 %>% 2 # prefer row 1 to row 2
#' @param  a The preferred row
#' @param  b The nonpreferred row
#' @export
#' @family Preference statement tools
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
#' @family Preference statement tools
#' @export
`%<%` <- function(a,b){
  ret <- NULL
  # Need to check if function existed already, i.e. in dplyr
  if (existsFunction("%<%")) {
    oldLt <- `%<%`
    if(class(a) == class(b) & class(a) %in% c("numeric", "character")){
      ret <- list(b,a)
    } else {
      return(oldLt(a,b))
    }
  } else {
    ret <- list(b,a)
  }
  class(ret) <- c("list", "strict")
  return(ret)
}


#' A helper function to add in preferences in a user-friendly way 
#' @examples 1 %<% 2 # prefer row 2 to row 1
#' @param  b The preferred row
#' @param  a The nonpreferred row
#' @family Preference statement tools
#' @export
`%=%` <- function(a,b){
  ret <- NULL
  # Need to check if function existed already, i.e. in dplyr
  if (existsFunction("%=%")) {
    oldEq <- `%=%`
    if(class(a) == class(b) & class(a) %in% c("numeric", "character")){
      ret <- (list(a,b))
    } else {
      return(oldEq(a,b))
    }
  } else {
    ret <- (list(a,b))
  }
  class(ret) <- c("list", "indif")
  return(ret)
}

#' A convinience function for generating Normal priors
#' @examples Normal(0, 1)(1) == dnorm(1)
#' @param  mu The mean of the normal distribution
#' @param  sigma The standard deviation of the prior
#' @family Priors
#' @export
#' @return A function yielding the PDF a x of a normal distribution with given statistics.
Normal <- function(mu, sigma) function(x) dnorm(x, mu, sigma)

#' A convinience function for generating Exponential priors
#' @examples Exp(1)(1) == dexp(1,1)
#' @param  mu The mean of the exponential distribution
#' @family Priors
#' @export
#' @return A function yielding the PDF a x of a exponential distribution with given statistics.
Exp <- function(mu) function(x) dexp(x, 1.0 / mu)
