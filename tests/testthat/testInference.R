data <- as.matrix(data.frame(x = c(1, 0, 1), 
                             y = c(0, 1, 1)))
priors <-  c(Normal(1, 0.5), 
             Exp(0.5))

p <- prefEl(data = data, priors = priors)

p$infer(estimate = "MAP")

df <- data.frame(x = 0.01 * c(0:100))

f <- function(x) BayesPref:::.calculateLogProb(c(x, 0.1), p)

df$y <- f(df$x)
