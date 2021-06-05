
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aggregator

<!-- badges: start -->
<!-- badges: end -->

Various personal datasets in a tidy format.

# How to update the data

To add a dataset `x`, create `data-raw/x.R` that exports the data with
`save(x, "data/x.rda")`. The path to the raw data is in
`data-raw/path_to_x.txt`.

Run or source `x.R` to create the `.rda` file. Then reinstall the
package with `devtools::install()`.
