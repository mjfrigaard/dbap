app_inputs_test <- tibble::tibble(
  # logical binary
  log_bin_na = bin_maker(
    type = "log",
    size = 10, missing = TRUE
  ),
  log_bin = bin_maker(
    type = "log",
    size = 10, missing = FALSE
  ),

  # integer binary
  int_bin_na = bin_maker(
    type = "int",
    size = 10, missing = TRUE
  ),
  int_bin = bin_maker(
    type = "int",
    size = 10, missing = FALSE
  ),

  # character binary
  chr_bin_na = bin_maker(
    type = "chr",
    size = 10, missing = TRUE
  ),
  chr_bin = bin_maker(
    type = "chr",
    size = 10, missing = FALSE
  ),
  chr_bin2_na = chr_maker(
    size = 10,
    lvls = 1, missing = TRUE
  ),

  # double
  dbl_var_na = dbl_maker(10, missing = TRUE),
  dbl_var = dbl_maker(size = 10),
  # integer
  int_var_na = int_maker(10, missing = TRUE),
  int_var = int_maker(size = 10),
  # character
  chr6_var = chr_maker(
    size = 10,
    lvls = 6, missing = TRUE
  ),
  chr7_var_na = chr_maker(size = 10, lvls = 7),
  # factor
  fct6_var_na = fct_maker(
    size = 10,
    lvls = 6, missing = TRUE
  ),
  fct7_var = fct_maker(size = 10, lvls = 7),
  # ordered
  ord6_var_na = fct_maker(
    size = 10, lvls = 6,
    ord = TRUE, missing = TRUE
  ),
  ord7_var = fct_maker(
    size = 10,
    lvls = 7, ord = TRUE
  ),

  # character facets
  chr_facet5 = facet_maker(
    type = "chr", size = 10,
    lvls = 5, missing = FALSE
  ),
  chr_facet5_na = facet_maker(
    type = "chr", size = 10,
    lvls = 5, missing = TRUE
  ),
  # factor facets
  fct_facet5 = facet_maker(
    type = "fct", size = 10,
    lvls = 5, missing = FALSE
  ),
  fct_facet5_na = facet_maker(
    type = "fct", size = 10,
    lvls = 5, missing = TRUE
  ),
  # ordered facets
  ord_facet5 = facet_maker(
    type = "ord", size = 10,
    lvls = 5, missing = FALSE
  ),
  ord_facet5_na = facet_maker(
    type = "ord", size = 10,
    lvls = 5, missing = TRUE
  )
)

# export to tests/testthat/fixtures/
saveRDS(app_inputs_test,
  file = "tests/testthat/fixtures/app_inputs_test.rds"
)
