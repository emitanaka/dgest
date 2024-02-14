test_that("markers works", {
    expect_equal(genotype_mismatch_left(toygeno$genotype, toypheno$genotype),
                 c("Jiglypuff", "Charmander"))
})
