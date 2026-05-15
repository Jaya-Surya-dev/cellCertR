test_that(
  "classify_confidence works",
  {
    scores <- c(
      0.2,
      0.6,
      0.9
    )

    result <- classify_confidence(
      scores
    )

    expect_length(
      result,
      3
    )

    expect_true(
      is.character(result)
    )
  }
)
