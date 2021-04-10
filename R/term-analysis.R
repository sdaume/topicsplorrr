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
    dplyr::group_by(.data$term) %>%
    dplyr::summarise(n_term = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(term_share = .data$n_term/sum(.data$n_term)) %>%
    dplyr::arrange(-.data$n_term)

  return(term_counts_shares)
}



#' Summarize mappings between terms and tokens from a text source
#'
#' \code{terms_tokens_map} summarizes the result returned by the
#' \code{\link{terms_by_date}} function(s). The primary purpose is to use this in
#' labelling results in for example plots.
#'
#' @param termsByDate a dataframe as returned by \code{\link{terms_by_date}}
#'
#' @return a dataframe with four columns:\describe{ \item{term}{a unique
#'   character sequence identified in a text source} \item{term_instances}{a
#'   list of unique instances of \code{term} (i.e. \code{token}s) that were
#'   reduced to \code{term}} \item{n_term_instances}{a list of respective counts
#'   of the term instances in \code{term_instances}} \item{term_label}{a
#'   suitable default label for the \code{term}} }
#'
#' @export
#'
terms_tokens_map <- function(termsByDate) {

  term_tokens <- termsByDate %>%
    dplyr::group_by(.data$term, .data$token) %>%
    dplyr::summarize(n_token_instances = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$term) %>%
    dplyr::arrange(-.data$n_token_instances, .data$token) %>%
    dplyr::mutate(term_instances = list(.data$token),
                  n_term_instances = list(.data$n_token_instances)) %>%
    dplyr::filter(dplyr::row_number() == 1) %>% # most frequent or first alphabetically
    dplyr::ungroup() %>%
    dplyr::rename(term_label = .data$token) %>%
    dplyr::select(-.data$n_token_instances) %>%
    dplyr::select(.data$term, .data$term_instances,
                  .data$n_term_instances, .data$term_label)

  return(term_tokens)
}




#' Compute the frequency of terms within in a chosen time period
#'
#' \code{term_frequencies} summarizes the counts and relative frequency of terms
#' in a chosen timebin for a given collection of terms with date of occurrence.
#'
#' Timebins for which no occurrence of a given term is recorded are added with
#' an explicit value of zero, excluding however such empty timebins before the
#' first occurrence of a term and after the last.
#'
#' @param termsByDate a dataframe as returned by \code{\link{terms_by_date}}
#'
#' @param timeBinUnit a character sequence specifying the time period that
#'   should be used as a bin unit when computing term frequencies. Valid values
#'   are \code{"day", "week", "month", "quarter", "year"}, but for the text
#'   sources processed in this package \code{"week"} is recommended and used as
#'   a default. \strong{NOTE:} for the assignment of \code{week}s Monday is
#'   considered as the first day of the week.
#'
#' @param minTermTimeBins a double in the range \code{[0,1]} specifying the
#'   minimum share of all unique timebins in which an occurrence of a term must
#'   have been recorded, i.e. a value of \code{0.5} (the default) requires that
#'   an occurrence of a term must have been recorded in at least 50\% of all
#'   unique timebins covered by the dataset; terms that do not meet this
#'   threshold will not be included in the returned results.
#'
#' @param minTermOccurences an integer specifying the minimum of total
#'   occurrences of a term to be included in the results; terms that do not meet
#'   this threshold will not be included in the returned results.
#'
#' @return a dataframe with term frequencies by chosen timebin, where:
#'   \describe{ \item{term}{a term as provided as an input in
#'   \code{termsByDate}} \item{timebin}{the first day of the a timebin; if
#'   \code{timeBinUnit} was set to \code{week}, this date will always be a
#'   Monday} \item{n_term_per_timebin}{the number of occurrences of \code{term}
#'   in \code{timebin}} \item{first_occur}{the exact date of the first
#'   occurrence of \code{term} across the whole time range covered by
#'   \code{timebin}s} \item{latest_occur}{the exact date of the latest
#'   occurrence of \code{term} across the whole time range covered by
#'   \code{timebin}s; note that this date can be larger than the maximum
#'   \code{timebin}, as \code{timebin} specifies the floor date of a time unit}
#'   \item{term_share_per_timebin}{the share of \code{term} in a given
#'   \code{timebin} with respect to all other term occurrences in a
#'   \code{timebin}; \strong{NOTE} that this is computed in consideration of all
#'   terms, including those that may be filtered out of the results}
#'   \item{n_term_total}{the total number of occurrences in the dataset, i.e.
#'   across all \code{timebin}s} \item{n_term_timebins}{the number of unique
#'   \code{timebin}s in which an occurrence of \code{term} was recorded} }
#'
#' @importFrom rlang .data
#' @export
#'
term_frequencies <- function(termsByDate, timeBinUnit = "week",
                             minTermTimeBins = 0.5, minTermOccurences = 10) {

  # candidates for additional function arguments
  week_start_day <- 1

  # compute the frequencies by timeunit
  term_freqs <- termsByDate %>%
    dplyr::select(.data$occur, .data$term) %>%
    dplyr::mutate(timebin = lubridate::floor_date(.data$occur,
                                                  unit = timeBinUnit,
                                                  week_start = week_start_day)) %>%
    dplyr::group_by(.data$term) %>%
    dplyr::mutate(first_occur = min(.data$occur),
                  latest_occur = max(.data$occur)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$term, .data$timebin) %>%
    dplyr::summarize(n_term_per_timebin = dplyr::n(),
                     first_occur = min(.data$occur),
                     latest_occur = max(.data$occur)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$timebin) %>%
    dplyr::mutate(#sum_terms_per_timebin = sum(n_term_per_timebin),
      term_share_per_timebin = .data$n_term_per_timebin /
        sum(.data$n_term_per_timebin)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$term) %>%
    dplyr::mutate(n_term_total = sum(.data$n_term_per_timebin),
                  n_term_timebins = dplyr::n_distinct(.data$timebin),
                  first_occur = min(.data$first_occur),
                  latest_occur = max(.data$latest_occur)) %>%
    dplyr::ungroup() #%>%
  #dplyr::select(-sum_terms_per_timebin)

  # filter out terms with low total counts or infrequent occurrence
  if (minTermTimeBins > 1) {
    warning("minTermTimeBins must be in (0,1]. ",
            "Using minTermTimeBins = 1 instead of ",
            minTermTimeBins)
    minTimeBins <- length(unique(term_freqs$timebin))
  } else if (minTermTimeBins == 0) {
    warning("minTermTimeBins must be in (0,1]. ",
            "Ignoring minTermTimeBins, all terms will be included in the result.")
    minTimeBins <- 1
  } else {
    minTimeBins <- ceiling(minTermTimeBins * length(unique(term_freqs$timebin)))
  }

  term_freqs <- term_freqs %>%
    dplyr::filter(.data$n_term_timebins >= minTimeBins) %>%
    dplyr::filter(.data$n_term_total >= minTermOccurences)

  # complement missing timebins per term with explicit zero counts
  # (important wrt to plotting and regression); filter out empty bins before the
  # first occurrence of a term, AND after the latest!
  if (nrow(term_freqs) > 1) {
    term_freqs <- term_freqs %>%
      tidyr::complete(.data$term,
                      timebin = seq.Date(min(.data$timebin),
                                         max(.data$timebin),
                                         by = timeBinUnit)) %>%
      #tidyr::complete(tidyr::crossing(term,
      #                                timebin = seq.Date(min(timebin),
      #                                                   max(timebin),
      #                                                   by = timeBinUnit))) %>%
      tidyr::replace_na(list(n_term_per_timebin = 0,
                             term_share_per_timebin = 0)) %>%
      dplyr::group_by(.data$term) %>%
      dplyr::mutate(first_occur = min(.data$first_occur, na.rm = TRUE),
                    latest_occur = max(.data$latest_occur, na.rm = TRUE)) %>%
      dplyr::mutate(n_term_total = max(.data$n_term_total, na.rm = TRUE),
                    n_term_timebins = max(.data$n_term_timebins, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$term) %>%
      dplyr::arrange(.data$timebin) %>%
      dplyr::mutate(term_cumsum = cumsum(.data$n_term_per_timebin)) %>%
      dplyr::filter(.data$term_cumsum > 0) %>%
      dplyr::filter(!(.data$term_cumsum == .data$n_term_total & .data$n_term_per_timebin == 0)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-.data$term_cumsum) %>%
      dplyr::arrange(-.data$n_term_total, .data$term)
  }

  # NOTE: just tidyr::complete(tidyr::crossing(term, timebin)) is slightly faster,
  # which may be relevant for very large datasets; however, in contrast to the
  # explicit date sequence assignment, this approach may miss dates (i.e. timebins
  # if they are not present at all in the dataset

  return(term_freqs)
}




#' Select top terms by count, trend metric and/or name pattern
#'
#' \code{select_top_terms} allows to select a specified number of top terms
#' based on miscellaneous properties of the term frequencies. This method is
#' typically used to select term frequency time series for plotting and
#' exploratory analysis. See the details of the function arguments for selection
#' options.
#'
#' @param termFrequencies a dataframe of \code{term} frequencies as returned by
#'   \code{term_frequencies()}
#'
#' @param topN the number of returned top terms meeting the selection criteria
#'   in \code{selectBy}
#'
#' @param selectBy the selection approach which determines the metric by which
#'   \code{term}s will be sorted to select the \code{topN} terms. Currently, the
#'   following options are supported: \describe{
#'   \item{most_frequent}{\strong{the default}, select terms based on the total
#'   number of occurrences} \item{trending_up}{select terms with largest upwards
#'   trend; internally this is measured by the slope of a simple linear
#'   regression fit to a \code{term}'s frequency series.}
#'   \item{trending_down}{select terms with largest downward trend; internally
#'   this is measured by the slope of a simple linear regression fit to a
#'   \code{term}'s frequency series.} \item{trending}{select terms with either
#'   largest upward or downward trend; internally this is measured by the
#'   absolute value of the slope of a simple linear regression fit to a
#'   \code{term}s frequency series.} \item{most_volatile}{select terms with the
#'   largest change throughout the covered time period; internally this is
#'   measured by the residual standard deviation of the linear model fit to a
#'   \code{term}'s time frequency series.} }
#'
#' @param selectTerms a character vector of term patterns, that terms are
#'   matched to for selection. \code{regular expression} syntax can be applied,
#'   e.g. if \code{c("^mod", "an", "el$", "^outbreak$")} is supplied for
#'   \code{selectTerms}, all terms that \strong{either} start with \emph{'mod'}
#'   \strong{or} contain \emph{'an'} \strong{or} end with \emph{'el'} or the
#'   exact term \emph{'outbreak'} are matched. The arguments \code{selectBy} and
#'   \code{selectTerms} can be combined.
#'
#' @return a dataframe specifying trend metrics employed for selecting top
#'   \code{term}s, where: \describe{ \item{term}{a unique term}
#'   \item{n_term_total}{the total number of a \code{term}'s occurrences in the
#'   dataset} \item{slope}{the slope coefficient of a linear model fit to this
#'   \code{term}'s time frequency series} \item{volatility}{the residual
#'   standard deviation of a linear model fit to this \code{term}'s time
#'   frequency series} \item{trend}{a categorisation of the term frequency
#'   trend} }
#'
#' @export
#'
select_top_terms <- function(termFrequencies, topN = 25,
                             selectBy = "most_frequent",
                             selectTerms = NULL) {

  # check selection options
  validSelectByOptions <- c("most_frequent", "trending", "trending_up",
                            "trending_down", "most_volatile")

  if (!(selectBy %in% validSelectByOptions)) {
    stop("'", selectBy, "' is not a valid option for selectBy. ",
         "Use one of: ", paste(validSelectByOptions, collapse = ", "))
  }

  # we split the selection process into different path for efficiency
  top_n_terms <- termFrequencies %>%
    dplyr::select(.data$term, .data$n_term_total) %>%
    dplyr::distinct()

  # if term patterns are supplied we filter by those first
  if (!is.null(selectTerms) & length(selectTerms) > 0) {
    term_pattern <- paste(selectTerms, collapse = "|")

    top_n_terms <- top_n_terms %>%
      dplyr::filter(stringr::str_detect(.data$term, pattern = term_pattern))
  }

  # again for efficiency we handle "most_frequent" as a special case
  if(selectBy == "most_frequent") {
    top_n_terms <- top_n_terms %>%
      dplyr::arrange(-.data$n_term_total) %>%
      dplyr::slice(1:topN)
  }

  # for all other selection options and the final data we need the trend model
  term_trends_lm <- termFrequencies %>%
    dplyr::filter(.data$term %in% top_n_terms$term) %>%
    .term_trends()

  # first we select according to the chosen metric (specified in 'selectBy')
  top_n_terms <- top_n_terms %>%
    merge(term_trends_lm, by = "term")

  if(selectBy == "most_volatile") {
    top_n_terms <- dplyr::arrange(top_n_terms, -.data$volatility)
  } else if(selectBy == "trending") {
    top_n_terms <- dplyr::arrange(top_n_terms, -abs(.data$slope))
  } else if(selectBy == "trending_up") {
    top_n_terms <- dplyr::arrange(top_n_terms, -.data$slope)
  } else if(selectBy == "trending_down") {
    top_n_terms <- dplyr::arrange(top_n_terms, .data$slope)
  }

  top_n_terms <- dplyr::slice(top_n_terms, 1:topN)

  return(top_n_terms)
}




#' Fit a linear regression to time frequency series of term shares
#'
#' \code{.term_trends} takes a dataframe with term frequency time series as
#' returned by the \code{term_frequencies()} function and fits a linear
#' regression for each \code{term}.
#'
#' The primary purpose of this function is to provide a simple approach to
#' identify \code{term} trends, which can be used for visualization, filtering
#' and exploratory analysis. Internally, the slope and intercept of the fitted
#' linear model are use to categorize a basic trend for the term frequencies.
#'
#' @param termFrequencies a dataframe of \code{term} frequencies as returned by
#'   \code{term_frequencies()}
#'
#' @param trendThreshold a double used to categorise trends, default is
#'   \code{0.0005}; if the intercept of the linear model of term frequencies
#'   falls within a range of \code{+/-trendThreshold} of the mean term share,
#'   the trend is categorized as \emph{"constant"}
#'
#' @return a dataframe specifying trend metrics for each \code{term}, where:
#'   \describe{ \item{term}{a unique term} \item{slope}{the slope
#'   coefficient of the linear model fit to this \code{term}'s time frequency
#'   series} \item{volatility}{the residual standard deviation of the linear
#'   model fit to this \code{term}'s time frequency series, which is used as a
#'   basic measure of volatility of term frequencies} \item{trend}{a
#'   categorisation of the term frequency trend, negative slopes with an
#'   intercept \code{> mean term share + trendThreshold} are interpreted as
#'   \emph{decreasing}, positive slopes with an intercept \code{< mean term
#'   share - trendThreshold} as \emph{increasing}, all others as
#'   \emph{constant}} }
#'
#' @keywords internal
#'
.term_trends <- function(termFrequencies, trendThreshold = 0.0005) {

  # index the timebin for fitting the lm; in order to get interpretable
  # intercepts when categorizing trends
  termFrequencies <- termFrequencies %>%
    dplyr::mutate(binindex = dplyr::dense_rank(.data$timebin) - 1)

  # linear model fit for each term
  lm_terms <- lapply(split(termFrequencies, termFrequencies$term),
                     stats::lm,
                     formula = term_share_per_timebin ~ binindex)

  term_slope <- as.data.frame(t(sapply(lm_terms, stats::coefficients)))
  #term_slope[1] <- NULL
  colnames(term_slope) <- c("intercept", "slope")

  # use residual standard deviation as a basic measure of volatility
  term_sigma <- data.frame(volatility = sapply(lm_terms, stats::sigma))

  # merge and categorise according to a threshhold/band
  term_lm_trends <- merge(term_slope, term_sigma, by=0)
  colnames(term_lm_trends)[1] <- c("term")

  term_lm_trends <- termFrequencies %>%
    dplyr::group_by(.data$term) %>%
    dplyr::summarize(mean_termshare = mean(.data$term_share_per_timebin)) %>%
    dplyr::ungroup() %>%
    merge(term_lm_trends, by = "term") %>%
    dplyr::mutate(trend = dplyr::case_when(
      .data$mean_termshare - .data$intercept > trendThreshold ~ "increasing",
      .data$mean_termshare - .data$intercept < -trendThreshold ~ "decreasing",
      TRUE ~ "constant")) %>%
    dplyr::select(-.data$mean_termshare)

  return(term_lm_trends)
}


