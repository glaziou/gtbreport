#' Number formatter according to GTB rounding rules
#'
#' Formats vectors of numbers (<2 billion)
#'
#' GTB rounding convention:
#'
#' - 0 is written as "0" (output "0")
#'
#' - values under 0.1 are written "<0.1" ("<0.1")
#'
#' - from 0.1 to under 1 are rounded to 2
#'
#'   significant figures (0.NN)
#'
#' - from 1 to under 10 are rounded to 2 significant figures ("N.N")
#'
#' - 10 to under 100 are rounded to 2 significant figures ("NN")
#'
#' - 100 to under 1000 are rounded to 3 significant figures ("NNN")
#'
#' - 1000 upwards are rounded to 3 significant figures ("N NN0 000")
#'
#' - data that are not reported, but could be are represented
#'   as empty cells and should be accompanied by a footnote.
#'
#' - data that cannot be calculated, either because of missing
#'   data, data was not requested, or any other reason are represented
#'   with a dash.
#'
#' When the number represents thousands, show numbers between 0.01 and 0.1:
#'
#' - values under 0.01 are written "<0.01"
#'
#' - values between 0.01 and under 0.1 are rounded to
#'   2 significant figures ("0.0NN")
#'
#' @param x Vector of numbers
#' @examples
#' ftb(348838)
#' ftb(c(0.0359, 0.00036))
#'
#' @export
#'
ftb <- Vectorize(function(x) {
  # formatter according to GTB rounding rules
  # https://docs.google.com/document/d/1cu_syknBiF3scAX9d7hEfN0LZwdG40i8ttN6yua2xTQ/edit
  #' @param x vector of values
  #' @export
  stopifnot(!is.character(x))
  stopifnot(x < 2e9)

  if (!is.na(x)) {
    smallpos <- x > 0 & x < 0.01
    one2ten <- x >= 1 & x < 10
    zero2one <- x >= 0.1 & x < 1

    dg <- ifelse(abs(x) > 0.01 & abs(x) < 100, 2, 3)
    x2 <- signif(x, dg)

    trailing.0 <- x2 == round2(x) & one2ten == TRUE
    trailing0 <- x2 * 10 == round2(x * 10) & zero2one == TRUE

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



int2word <- function(x) {
  #' Converts integers into English words,
  #' Based on a function by John Fox:
  #' http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  #' @param x an integer
  #' @examples
  #' int2word(324513)
  #' @export

  if (x == 0) {
    print("zero")
  } else{
    helper <- function(x) {
      digits <- rev(strsplit(as.character(x), "")[[1]])
      nDigits <- length(digits)
      if (nDigits == 1)
        as.vector(ones[digits])
      else if (nDigits == 2)
        if (x <= 19)
          as.vector(teens[digits[1]])
      else
        trim(paste(tens[digits[2]],
                   Recall(as.numeric(digits[1]))))
      else if (nDigits == 3)
        trim(paste(ones[digits[3]], "hundred and",
                   Recall(makeNumber(digits[2:1]))))
      else {
        nSuffix <- ((nDigits + 2) %/% 3) - 1
        if (nSuffix > length(suffixes))
          stop(paste(x, "is too large!"))
        trim(paste(
          Recall(makeNumber(digits[nDigits:(3 * nSuffix + 1)])),
          suffixes[nSuffix],
          "," ,
          Recall(makeNumber(digits[(3 * nSuffix):1]))
        ))
      }
    }
    trim <- function(text) {
      # Tidy leading/trailing whitespace, space before comma
      text = gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,", ",", text)))
      # Clear any trailing " and"
      text = gsub(" and$", "", text)
      # Clear any trailing comma
      gsub("\ *,$", "", text)
    }
    makeNumber <-
      function(...)
        as.numeric(paste(..., collapse = ""))
    #Disable scientific notation
    opts <- options(scipen = 100)
    on.exit(options(opts))
    ones <-
      c("",
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine")
    names(ones) <- 0:9
    teens <-
      c(
        "ten",
        "eleven",
        "twelve",
        "thirteen",
        "fourteen",
        "fifteen",
        "sixteen",
        "seventeen",
        "eighteen",
        "nineteen"
      )
    names(teens) <- 0:9
    tens <-
      c("twenty",
        "thirty",
        "forty",
        "fifty",
        "sixty",
        "seventy",
        "eighty",
        "ninety")
    names(tens) <- 2:9
    x <- round(x)
    suffixes <- c("thousand", "million", "billion", "trillion")
    if (length(x) > 1)
      return(trim(sapply(x, helper)))
    helper(x)
  }

}


