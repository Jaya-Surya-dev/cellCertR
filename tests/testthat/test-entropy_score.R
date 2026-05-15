test_that(
  "entropy_score returns numeric values",
  {
    mat <- matrix(
      runif(20),
      nrow = 5
    )

    result <- entropy_score(
      mat
    )

    expect_type(
      result,
      "double"
    )

    expect_length(
      result,
      5
    )
  }
)
