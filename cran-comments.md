## Test environments
* local OS X install, R 3.6.3
* ubuntu 14.04 (on travis-ci), R 3.6.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

Previous version was using `mutate()` function from `dplyr` package. Given that a new version of `dplyr` is going to be relaeased shortly and the use of `mutate()` function is changing and it is not compatible anymore, I changed the code so that it does not rely anymore on `mutate()`. 

