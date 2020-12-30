#' Replaces outliers from a vector and returns a new vector
#'
#' @param col_in The vector
#' @param my_prob Probability of quantiles (will remove quantiles at p and 1-p)
#'
#' @return A vector without the outliers
#' @export
#'
#' @examples
#' x <- afedR_replace_outliers(runif(100))
afedR_replace_outliers <- function(col_in, my_prob = 0.05) {

  # return if class is other than numeric
  if (!(class(col_in) %in% c('numeric', 'integer'))) return(col_in)

  my_outliers <- stats::quantile(x = col_in,
                                 probs = c(my_prob, 1-my_prob),
                                 na.rm = TRUE)

  idx <- (col_in <= my_outliers[1])|(col_in >= my_outliers[2])
  col_in[idx] <- NA

  return(col_in)

}

#' Replaces outliers in all numeric columns of a dataframe
#'
#' @param df_in The dataframe
#' @param my_prob Probability of quantiles (will remove quantiles at p and 1-p)
#'
#' @return A dataframe without the outliers in all numeric columns
#' @export
#'
#' @examples
#' x <- afedR_replace_outliers(runif(100))
afedR_replace_outliers_df <- function(df_in, my_prob = 0.05) {

  l_out <- purrr::map(df_in, afedR_replace_outliers, my_prob = my_prob)

  # rebuild dataframe
  df_out <- dplyr::as_tibble(l_out)

  return(df_out)

}


#' Calculates a return vector
#'
#' @param P Vector of prices
#' @param tickers Vector of tickers
#'
#' @return A vector of returns
#' @export
#'
#' @examples
#' print(afedR_calc_ret(1:10))
afedR_calc_ret <- function(P,
                     tickers = rep('ticker', length(P))) {
  # calculates arithmetic returns from a vector of prices
  #
  # Args:
  #   P - vector of prices (numeric)
  #   tickers - vector of tickers (optional)
  #
  # Returns:
  #   A vector of returns

  # ret_t = p_{t}/p_{t-1} - 1

  # error checking
  if ( !(class(P) %in% c('numeric', 'integer'))) {
    stop('P should be a numeric object.')
  }

  if ( !(class(tickers) %in% c('character', 'factor'))) {
    stop('tickers should be a character or factor object.')
  }

  if (length(P) != length(tickers)) {
    stop('The length of P and tickers does not match.')
  }

  if ( length(P) < 2) {
    stop('input P should have at least 2 elements.')
  }

  my_length <- length(P)
  ret <- c(NA, P[2:my_length]/P[1:(my_length - 1)] - 1)

  idx <- (tickers != c(NA, tickers[1:(my_length-1)]))
  ret[idx] <- NA

  return(ret)
}
