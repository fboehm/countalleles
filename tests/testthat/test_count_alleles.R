#devtools::use_testthat()

library(countalleles)
context("count_alleles gives outputs of integers 0,1,2 and of same length as inputted vector")

test_that("count_alleles gives output of same length as input", {
    expect_equal(length(c("TC", "CC", "TT")), length(count_alleles(c("TC", "CC", "TT"))))
    expect_equal(length(rep(c("TC", "CC", "TT"),10)), length(count_alleles(rep(c("TC", "CC", "TT"),10))))})

nums <- 0:2

test_that("count_alleles gives outputs of integers 0,1,and 2", {
    expect_true(identical(rep(TRUE, length(c("TC", "CC", "TT"))),(count_alleles(c("TC", "CC", "TT")) %in% nums)))
    expect_true(all.equal(rep(TRUE, length(c("AG", "AA", "GG"))),(count_alleles(c("AG", "AA", "GG")) %in% nums)))})

