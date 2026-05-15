test_that(
  "calibrate_confidence works",
  {
    scores <- c(
      0.2,
      0.5,
      0.8
    )

    result <- calibrate_confidence(
      scores
    )

    expect_length(
      result,
      3
    )

    expect_true(
      all(result >= 0)
    )

    expect_true(
      all(result <= 1)
    )
  }
)
