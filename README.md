
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `dbap`

<!-- badges: start -->
<!-- badges: end -->

The goal of `dbap` is to demonstrate how to test a shiny application
package using [`testthat`](https://testthat.r-lib.org/) and
[`shiny::testServer()`](https://search.r-project.org/CRAN/refmans/shiny/html/testServer.html)

## Installation

You don’t want to install this package, but you might want to download
it as an example (or read through [this
post](https://mjfrigaard.github.io/posts/test-shiny-p2/) to learn about
it’s contents).

## Utility function tests

``` r
source("tests/testthat/helpers.R")
testthat::test_file("tests/testthat/test-get_col_types.R")
```

    [ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ]

``` r
testthat::test_file("tests/testthat/test-check_binary_vec.R")
```

    [ FAIL 0 | WARN 0 | SKIP 0 | PASS 25 ]

``` r
testthat::test_file("tests/testthat/test-check_facet_vec.R")
```

    [ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ]

``` r
testthat::test_file("tests/testthat/test-make_binary_vec.R")
```

    [ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ]

``` r
testthat::test_file("tests/testthat/test-make_facet_vec.R")
```

    [ FAIL 0 | WARN 0 | SKIP 0 | PASS 7 ]

``` r
testthat::test_file("tests/testthat/test-pull_numeric_cols.R")
```

    [ FAIL 0 | WARN 0 | SKIP 0 | PASS 1 ]

``` r
testthat::test_file("tests/testthat/test-pull_binary_cols.R")
```

    [ FAIL 0 | WARN 0 | SKIP 0 | PASS 1 ]

``` r
testthat::test_file("tests/testthat/test-pull_facet_cols.R")
```

    [ FAIL 0 | WARN 0 | SKIP 0 | PASS 1 ]

``` r
testthat::test_file("tests/testthat/test-pull_cat_cols.R")
```

    [ FAIL 0 | WARN 0 | SKIP 0 | PASS 1 ]

# Shiny server tests

Check the shiny `testServer()` tests for the modules in
`tests/testthat/`

    #> tests/testthat/
    #> ├── test-mod_cols_server.R
    #> ├── test-mod_ds_server.R
    #> ├── test-mod_pkg_server.R
    #> └── test-mod_plot_server.R

``` r
# run module function tests -----------------------------------------------
testthat::test_file("tests/testthat/test-mod_pkg_server.R")
testthat::test_file("tests/testthat/test-mod_ds_server.R")
testthat::test_file("tests/testthat/test-mod_cols_server.R")
testthat::test_file("tests/testthat/test-mod_plot_server.R")
```
