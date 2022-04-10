## Test environments

-   ubuntu-latest, R-4.1.3 and R-devel
-   macos-latest, R-4.1.3 and R-devel
-   windows-latest, R-4.1.3 and R-devel

## R CMD check results

Ubuntu/MacOS (All R-versions) and Windows (R-4.1.3):

    0 errors | 0 warnings | 0 note

Windows (R-devel): 

```
0 errors | 1 warnings | 0 note

Warning: package 'mcmc' was built under R version 4.1.3
See 'C:/Users/USERuQGXYhMLJB/prefeR.Rcheck/00install.out' for details
```

The "mcmc" package is one of this package's dependencies which was not automatically rebuilt under the R-development image.

## Reverse dependencies

As of submission, no other packages list this as a dependency.

## This is a minor update of prefeR.

This change updates the existing package to work with R version 4.1.3 and higher.
