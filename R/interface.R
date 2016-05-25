# Define a class to hold all of our data
prefEl <- setRefClass("BayesPrefClass",
                     fields = c("data",
                                "priors",
                                "sigma",
                                "strict",
                                "indif",
                                "sense"), 
                     methods = list(
                       initialize = function(...){
                         # Provide default values
                         data   <<- NA
                         priors <<- list()
                         sigma  <<- 0.1
                         strict <<- list()
                         indif  <<- list()
                         sense  <<- "min"
                         # Call super to override any defaults
                         callSuper(...)
                       }, 
                       show = function(){
                         cat("Preference elicitation object with:\n")
                         
                         # Number of observations
                         if (is.null(nrow(data))){
                           cat("\t0 Rows of Data\n")
                         } else {
                           cat(paste0("\t", nrow(data), "observations of ", ncol(data), " data\n"))
                         }
                         
                         # Strict preferences
                         if (length(strict) == 0){
                           cat("\tNo strict preferences.\n")
                         } else if (length(strict) < 10){
                           for (x in strict){
                             cat(paste(x[1], "preferred to", x[2], "\n"))
                           } 
                         } else {
                           cat(paste(length(strict), "strict preferences.\n"))
                         }
                         
                         # Indif preferences
                         if (length(indif) == 0){
                           cat("\tNo indifference preferences.\n")
                         } else if (length(indif) < 10){
                           for (x in indif){
                             cat(paste(x[1], "indifferent to", x[2], "\n"))
                           } 
                         } else {
                           cat(paste(length(indif), "indifference preferences.\n"))
                         }
                         
                       }
                     ))