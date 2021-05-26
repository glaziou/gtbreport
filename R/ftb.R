#' Number formatter according to GTB rounding rules
#'
#' Formats vectors of numbers (<2 billion)
#'
#' @param x Vector of numbers
#'
#' @export
#'
ftb <- Vectorize(function(x) {
  # formatter according to GTB rounding rules
  # https://docs.google.com/document/d/1cu_syknBiF3scAX9d7hEfN0LZwdG40i8ttN6yua2xTQ/edit
  #' @param x vector of values
  #' @export
  stopifnot(!is.character(x))
  stopifnot(x<2e9)

  if (!is.na(x)) {
    smallpos <- x > 0 & x < 0.01
    one2ten <- x >= 1 & x < 10
    zero2one <- x >= 0.1 & x < 1

    dg <- ifelse(abs(x) > 0.01 & abs(x) < 100, 2, 3)
    x2 <- signif(x, dg)

    trailing.0 <- x2==as.integer(x) & one2ten==TRUE
    trailing0 <- x2*10==as.integer(x * 10) & zero2one==TRUE

    x2 <-
      format(
        x2,
        digits = dg,
        nsmall = 0L,
        big.mark = " ",
        justify = 'right',
        drop0trailing = T,
        scientific = F
      )
    if (smallpos)
      x2 <- '<0.01'
    if (trailing.0)
      x2 <- paste0(x2, '.0')
    if (trailing0)
      x2 <- paste0(x2, '0')
  } else
    x2 <- '-'
  return(x2)
}, 'x')




