## Test environments

-   ubuntu-latest, R-4.1.3 and R-devel
-   macos-latest, R-4.1.3 and R-devel
-   windows-latest, R-4.1.3 and R-devel

## R CMD check results

All Configurations:
    0 errors | 0 warnings | 0 note

## Reverse dependencies

As of submission, no other packages list this as a dependency.

## This is a minor update of prefeR.

This change updates the existing package to work with R version 4.1.3 and higher.

## Previous submission comments:
package prefeR_0.1.2.tar.gz does not pass the incoming checks automatically, please see the following pre-tests:
Windows: <https://win-builder.r-project.org/incoming_pretest/prefeR_0.1.2_20220411_013008/Windows/00check.log>
Status: 2 NOTEs
Debian: <https://win-builder.r-project.org/incoming_pretest/prefeR_0.1.2_20220411_013008/Debian/00check.log>
Status: 1 NOTE

This revision fixes the above issues by:
* Updating the hyperlinks in the README.md file so they point to valid addresses.
* Tuning an example so that it runs much faster so we don't encounter timeouts on the windows builds.
