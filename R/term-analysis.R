#' Count unique terms in a text source
#'
#' \code{term_counts} counts the occurences and computes shares of unique
#' \code{term}s in a text source. Expects the result of
#' \code{\link{terms_by_date}} or its variations as input.
#'
#' @param termsByDate a dataframe as returned by \code{\link{terms_by_date}}
#'
#' @return a dataframe with three columns listing all unique \code{term}s in the
#'   provided text source, where \code{n_term} is the total number of occurences
#'   in the text source and \code{term_share} the share relative to all other
#'   terms; if no stemming has been applied the term and token in the result are
#'   identical
#'
#' @export
#'
term_counts <- function(termsByDate) {

  term_counts_shares <- termsByDate %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(n_term = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(term_share = n_term/sum(n_term)) %>%
    dplyr::arrange(-n_term)

  return(term_counts_shares)
}

