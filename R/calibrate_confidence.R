#' Calibrate Confidence Scores
#'
#' Converts raw confidence scores
#' into calibrated percentile scores.
#'
#' @param scores Numeric vector
#'
#' @return Numeric vector
#'
#' @examples
#' scores <- c(
#'   0.2,
#'   0.5,
#'   0.8
#' )
#'
#' calibrate_confidence(
#'   scores
#' )
#'
#' @export

calibrate_confidence <- function(
  scores
) {
  ranks <- rank(
    scores,
    ties.method = "average"
  )

  calibrated <- ranks /
    max(ranks)

  return(calibrated)
}
