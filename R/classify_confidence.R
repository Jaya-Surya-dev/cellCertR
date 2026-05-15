#' Classify Confidence Levels
#'
#' Converts confidence scores into
#' confidence classes.
#'
#' @param scores Numeric vector
#'
#' @return Character vector
#'
#' @examples
#' scores <- c(
#'   0.2,
#'   0.7,
#'   0.9
#' )
#'
#' classify_confidence(
#'   scores
#' )
#'
#' @export

classify_confidence <- function(
  scores
) {
  classes <- ifelse(
    scores >= 0.8,
    "High",
    ifelse(
      scores >= 0.5,
      "Moderate",
      "Low"
    )
  )

  return(classes)
}
