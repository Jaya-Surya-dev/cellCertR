#' Cell Annotation Confidence
#'
#' Main confidence scoring framework.
#'
#' @param object Seurat object
#' @param markers Named marker list
#' @param label_column Metadata label column
#'
#' @return Seurat object
#'
#' @examples
#' NULL
#'
#' @export

cell_certify <- function(
  object,
  markers,
  label_column = "predicted_label"
) {
  if (
    !label_column %in%
      colnames(
        object@meta.data
      )
  ) {
    stop(
      sprintf(
        "%s not found in metadata",
        label_column
      )
    )
  }

  if (
    !"entropy_norm" %in%
      colnames(
        object@meta.data
      )
  ) {
    warning(
      "entropy_norm not found. Using zeros."
    )

    object$entropy_norm <- 0
  }

  if (
    !"doublet_score" %in%
      colnames(
        object@meta.data
      )
  ) {
    warning(
      "doublet_score not found. Using zeros."
    )

    object$doublet_score <- 0
  }

  object$marker_score <-
    marker_score(
      object,
      markers,
      label_column
    )

  object$neighbor_score <-
    neighbor_score(
      object,
      label_column =
        label_column
    )

  entropy_component <-
    1 - object$entropy_norm

  confidence <-
    0.35 * scale(
      object$marker_score
    ) +
    0.35 * scale(
      object$neighbor_score
    ) +
    0.20 * scale(
      entropy_component
    ) -
    0.10 * scale(
      object$doublet_score
    )

  confidence <- as.numeric(
    scale(confidence)
  )

  confidence <- (
    confidence -

      min(
        confidence,
        na.rm = TRUE
      )
  ) / (
    max(
      confidence,
      na.rm = TRUE
    ) -

      min(
        confidence,
        na.rm = TRUE
      )
  )

  object$confidence_score <-
    calibrate_confidence(
      confidence
    )

  object$confidence_class <-
    classify_confidence(
      object$confidence_score
    )

  return(
    object
  )
}
