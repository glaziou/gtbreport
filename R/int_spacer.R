#' Round a number to the nearest integer and
#' convert to a string with space as the
#' thousands separator
#'
#' @param x A number to be rounded and spaced
#' @return A string
#' @examples
#' int_spacer(23.4)
#' int_spacer(9876)
#' int_spacer(1234567.8)
#'
#' @export
#'
int_spacer <- function(x) {
  ifelse(is.na(x),
         NA,
         formatC(round(x,0),
                 big.mark=" ",
                 format="d")
  )
}
