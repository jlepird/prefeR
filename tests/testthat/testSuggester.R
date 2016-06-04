# Basic data for testing
data <- as.matrix(data.frame(x = c(1, 0, 1), 
                             y = c(0, 1, 1)))
priors <- c(Normal(0,1), 
            Normal(0,1))

p <- prefEl(data = data, priors = priors)

p$addPref(BayesPref::`%>%`(1,3))

p$addPref(BayesPref::`%>%`(2,3))

suggest(p)
