### Package Status
[![R](https://github.com/jlepird/prefeR/actions/workflows/r.yml/badge.svg)](https://github.com/jlepird/prefeR/actions/workflows/r.yml)
[![codecov.io](https://codecov.io/gh/jlepird/prefeR/coverage.svg?branch=master)](https://codecov.io/gh/jlepird/prefeR?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/prefeR)](https://cran.r-project.org/package=prefeR)

### What is preference elicitation?
Most real-world decisions must reconcile multiple, competing objectives. In buying a car, you might be concerned about cost, reliability, and performance, but before you can make a decision, you must establish the relative importance of these goals. A common mathematical approach to this problem is to define weights for each of these objectives. Although you might have a ballpark intuition for the weights, it is difficult to set them in a repeatable and defendable manner. 

Preference elicitation relieves some of this burden. Instead of determining the weights directly, you make a series of pairwise comparisons between alternatives: do you prefer car A, car B, or are you indifferent? Research has shown that these pairwise comparisons are far easier to make and much easier to justify than explicitly setting the weights directly.  This package implements a preference elicitation algorithm that takes your stated preferences and uses them to calculate an optimal set of weights. It can even suggest which comparisons you should make to get the most accurate weights with the fewest number of queries. 

Technical details about how this package works can be found in the article [here](http://arc.aiaa.org/doi/abs/10.2514/1.I010363). 

### Installation
This package is on CRAN, so you can install it directly through `install.packages()`.  
```R
install.packages("prefeR")
```

### Examples
#### Hello, World
```R
library(prefeR)

# Each column of data is a variable, i.e. objective, 
# and each row is an alternative.
p <- prefEl(data = data.frame(x = c(1, 0, 1), 
                              y = c(0, 1, 1)))
# Set the prior belief on the weights for objectives x and y
p$priors <- c(Normal(0, 1), 
              Normal(0, 1))

# Add in some pairwise preferences
p$addPref(1 %>% 3) # prefer row 1 to row 3
p$addPref(3 %<% 2) # prefer row 2 to row 3

# Run the inference
p$infer()   # returns that x and y are of equal importance

# What comparision should you make next?
p$suggest() # suggest compare 1 to 2
p$addPref(1 %=% 2)

# Re-run the infernence algorithm 
p$infer()   # maintains belief that 1 and 2 are equal
p$rank()    # calculates the value of all three alternatives
```
## More Examples

[Choosing a car from the `mtcars` dataset](https://jlepird.github.io/prefeR/inst/doc/mtcars.html)
