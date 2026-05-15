#' Entropy-Based Uncertainty Score
#'
#' Calculates entropy from
#' prediction score matrices.
#'
#' @param score_matrix Numeric matrix
#'
#' @return Numeric vector
#'
#' @examples
#' mat <- matrix(
#'   runif(20),
#'   nrow = 5
#' )
#'
#' entropy_score(
#'   mat
#' )
#'
#' @export

entropy_score <- function(score_matrix) {
  entropy_values <- apply(
    score_matrix,
    1,
    function(x) {
      probs <- x / sum(x)

      probs <- probs[probs > 0]

      ent <- entropy::entropy(probs)

      return(ent)
    }
  )

  return(entropy_values)
}
