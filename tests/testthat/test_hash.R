test_that("hash() and unhash() work", {
    values <- month.name
    keys0 <- hash(values)
    expect_true(!any(keys0 == values))

    idx <- grepl("^Ma", values)
    keys1 <- hash(values, except = "^Ma")
    expect_identical(keys1[idx], values[idx])
    expect_true(!any(keys1[!idx] == values[!idx]))

    expect_identical(unhash(keys0), values)
    expect_identical(unhash(keys1), values)
})
