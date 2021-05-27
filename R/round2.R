#' always round 0.5 up
#'
#' @param x A number to be rounded
#' @param digits Number of digits
#'
#' @export
#'
round2 <-
  function(x, digits = 0)
    sign(x) * trunc(abs(x) * 10 ^ digits + 0.5) / 10 ^ digits
