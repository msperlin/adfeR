#' formats cash
#'
#' @param x  numeric
#' @param type_cash BRL or USD
#'
#' @return cash string
#' @export
#'
#' @examples
format_cash <- function(x, type_cash = 'USD') {
  require(scales)

  if (type_cash == 'USD') {
    x.formatted <- dollar(x,
                          prefix = '$',
                          decimal.mark = '.',
                          big.mark = ',',
                          largest_with_cents = Inf)

  }
  if (type_cash == 'BRL') {
    x.formatted <- dollar(x,
                          prefix = 'R$',
                          decimal.mark = ',',
                          big.mark = '.',
                          largest_with_cents = Inf)
  }

  return(x.formatted)
}

#' Formats percent
#'
#' @param x a numeric
#'
#' @return percent string
#' @export
#'
#' @examples
format_percent <- function(x) {
  require(scales)

  x.formatted <- percent(x,
                         decimal.mark = ',',
                         big.mark = '.', accuracy = 0.01)

  return(x.formatted)
}

#' Formats a date
#'
#' @param x a date
#'
#' @return formatted date
#' @export
#'
#' @examples
format_date <- function(x) {

  x <- as.Date(x)
  x.formatted <- format(x, '%d/%m/%Y')

  return(x.formatted)
}

