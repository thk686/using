
<!-- README.md is generated from README.Rmd. Please edit that file -->
This package simplifies managing your search path in the context of R's namespaces.

``` r
using::using(using)
using(using::use)
list_using()
#> [1] "use"
formals(use)
#> $directive
stop_using(use)
list_using()
#> [1] "use"
using(my_use = using::use)
list_using()
#> [1] "my_use" "use"
formals(my_use)
#> $directive
stop_using(my_use)
```
