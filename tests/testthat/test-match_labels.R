test_that(
  "match_labels identifies labels",
  {
    result <- match_labels(
      "T_cells",
      c(
        "T_cell",
        "B_cell"
      )
    )

    expect_true(
      is.character(result)
    )
  }
)
