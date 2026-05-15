#' Explain Cell Annotation Confidence
#'
#' Provides interpretable explanation
#' for confidence scores.
#'
#' @param object Seurat object
#' @param cell_id Cell barcode
#'
#' @return Character vector
#'
#' @examples
#' NULL
#'
#' @export

explain_cell <- function(
  object,
  cell_id
) {
  meta <- object@meta.data[
    cell_id, ,
    drop = FALSE
  ]

  message(
    "Cell: ",
    cell_id,
    "\n"
  )

  message(
    "Predicted Label: ",
    meta$predicted_label
  )

  message(
    "Confidence Score: ",
    round(
      meta$confidence_score,
      3
    )
  )

  message(
    "Marker Score: ",
    round(
      meta$marker_score,
      3
    )
  )

  message(
    "Neighbor Agreement: ",
    round(
      meta$neighbor_score,
      3
    )
  )

  message(
    "Entropy: ",
    round(
      meta$entropy_norm,
      3
    ),
    "\n"
  )

  interpretation <- c()

  if (
    meta$confidence_score < 0.4
  ) {
    interpretation <- c(
      interpretation,
      "Low confidence annotation"
    )
  } else if (
    meta$confidence_score < 0.7
  ) {
    interpretation <- c(
      interpretation,
      "Moderate confidence annotation"
    )
  } else {
    interpretation <- c(
      interpretation,
      "High confidence annotation"
    )
  }

  if (
    meta$entropy_norm > 0.7
  ) {
    interpretation <- c(
      interpretation,
      "High uncertainty detected"
    )
  }

  if (
    meta$neighbor_score < 0.5
  ) {
    interpretation <- c(
      interpretation,
      "Local neighborhood disagreement"
    )
  }

  if (
    meta$marker_score < 0.2
  ) {
    interpretation <- c(
      interpretation,
      "Weak marker support"
    )
  }

  message(
    "Interpretation:"
  )

  for (
    i in seq_along(
      interpretation
    )
  ) {
    message(
      "- ",
      interpretation[i]
    )
  }

  return(
    interpretation
  )
}
