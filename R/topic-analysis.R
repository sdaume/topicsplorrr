#' Compute topic shares for a given time bin
#'
#' \code{topic_frequencies} summarizes the shares of topics in a chosen time
#' interval as per provided topic shares by document and date.
#'
#' A \code{stm} topic model provides for each document the likelihood
#' (\emph{gamma}) that it is generated from a specific topic; here we interprete
#' these as the share of a document attributed to this topic and then summarize
#' these shares per timebin to obtain the share of a topic across all documents
#' over time.
#'
#' The topic share or likelihood per document has to be above a threshold
#' specified by \code{minGamma}. A suitable threshold might consider the number
#' of topics and the average document size. An additional filtering option is
#' provided with \code{minTopicTimeBins}.
#'
#' Timebins for which no occurrence of a given topic is recorded are added with
#' an explicit value of zero, excluding however such empty timebins before the
#' first occurrence of a topic and after the last.
#'
#' @param topicsByDocDate a dataframe as returned by
#'   \code{\link{topics_by_doc_date}}
#'
#' @param timeBinUnit a character sequence specifying the time period that
#'   should be used as a bin unit when computing topic share frequencies. Valid
#'   values are \code{"day", "week", "month", "quarter", "year"}, \code{"week"}
#'   is used as a default. \strong{NOTE}, for the assignment of \code{week}s
#'   Monday is considered as the first day of the week.
#'
#' @param minGamma the minimum share of a topic per document to be considered
#'   when summarizing topic frequencies, topics with smaller shares per
#'   individual document will be ignored when computing topic frequencies. (In
#'   an \code{\link[stm:stm]{stm topic model}} the likelihood that a topic is
#'   generated from a topic is expressed by the value \emph{gamma}.) The default
#'   is \code{0.01}, but should be adjusted with view of the number of topics
#'   and the average length of a document.
#'
#' @param minTopicTimeBins a double in the range \code{[0,1]} specifying the
#'   minimum share of all unique timebins in which an occurrence of a topic
#'   share of at least \code{minGamma} must have been recorded, i.e. a value of
#'   \code{0.5} (the default) requires that an occurrence of a topic must have
#'   been recorded in at least 50\% of all unique timebins covered by the
#'   dataset; topics that do not meet this threshold will not be included in the
#'   returned results.
#'
#' @return a dataframe with term frequencies by chosen timebin, where:
#'   \describe{ \item{topic_id}{a topic ID as provided as an input in
#'   \code{topicsByDocDate}} \item{timebin}{the floor date of a timebin; if
#'   \code{timeBinUnit} was set to \code{week}, this date will always be a
#'   Monday} \item{median_gamma}{the median of likelihoods of the topic with
#'   \code{topic_id} in \code{timebin}} \item{mean_gamma}{the mean of
#'   likelihoods of the topic with \code{topic_id} in \code{timebin}}
#'   \item{topicshare}{the share of topic with \code{topic_id} relative to all
#'   topic shares recorded and included in a given \code{timebin}.
#'   \strong{NOTE:} strictly speaking these are the likelihoods that a document
#'   is generated from a topic, which we here interpret as the share of a
#'   document attributed to a topic.} \item{n_docs_topic}{the total number of
#'   documents in a dataset in which a topic with \code{topic_id} occurs as
#'   least with likelihood \code{minGamma}} \item{first_occur}{the exact date of
#'   the first occurrence of a topic with \code{topic_id} across the whole time
#'   range covered by \code{timebin}s} \item{latest_occur}{the exact date of the
#'   latest occurrence of a topic with \code{topic_id} across the whole time
#'   range covered by \code{timebin}s; note that this date can be larger than
#'   the maximum \code{timebin}, as \code{timebin} specifies the floor date of a
#'   time unit} \item{n_topic_timebins}{the number of unique \code{timebin}s in
#'   a topic with \code{topic_id} occurs at least with likelihood
#'   \code{minGamma}} }
#'
#' @export
#'
topic_frequencies <- function(topicsByDocDate, timeBinUnit = "week",
                              minGamma = 0.01, minTopicTimeBins = 0.5) {
  # potential additional arguments
  weekStart <- 1

  # remove topic occurrences with low likelihood
  topic_freqs <- topicsByDocDate %>%
    dplyr::filter(.data$gamma >= minGamma)

  # create timebins and summarise the topic shares and add some other stats
  topic_freqs <- topic_freqs %>%
    dplyr::mutate(timebin = lubridate::floor_date(.data$occur,
                                                  unit = timeBinUnit,
                                                  week_start = weekStart)) %>%
    dplyr::group_by(.data$topic_id) %>%
    dplyr::mutate(n_docs_topic = dplyr::n(),
                  first_occur = min(.data$occur),
                  latest_occur = max(.data$occur)) %>%
    dplyr::ungroup() %>%

    dplyr::group_by(.data$timebin) %>%
    dplyr::mutate(sum_gamma = sum(.data$gamma),
                  n_docs_timebin = dplyr::n_distinct(.data$document)) %>%
    dplyr::ungroup() %>%

    dplyr::group_by(.data$timebin, .data$topic_id) %>%
    dplyr::mutate(gamma_share = .data$gamma/.data$sum_gamma) %>%
    dplyr::ungroup() %>%

    dplyr::group_by(.data$timebin, .data$topic_id, .data$n_docs_topic) %>%
    dplyr::summarize(median_gamma = stats::median(.data$gamma),
                     mean_gamma = mean(.data$gamma),
                     topicshare = sum(.data$gamma_share),
                     first_occur = min(.data$first_occur),
                     latest_occur = max(.data$latest_occur)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$topic_id) %>%
    dplyr::mutate(n_topic_timebins = dplyr::n_distinct(.data$timebin)) %>%
    dplyr::ungroup()

  # filter out topics with infrequent occurrence
  if (minTopicTimeBins > 1) {
    warning("minTopicTimeBins must be in (0,1]. ",
            "Using minTopicTimeBins = 1 instead of ",
            minTopicTimeBins)
    minTopicBins <- length(unique(topic_freqs$timebin))
  } else if (minTopicTimeBins == 0) {
    warning("minTopicTimeBins must be in (0,1]. ",
            "Ignoring minTopicTimeBins, all topics will be included in the result.")
    minTopicBins <- 1
  } else {
    minTopicBins <- ceiling(minTopicTimeBins * length(unique(topic_freqs$timebin)))
  }

  topic_freqs <- dplyr::filter(topic_freqs, .data$n_topic_timebins >= minTopicBins)

  # complement missing timebins per topic with explicit zero counts
  # (important wrt to plotting and regression); filter out empty bins before the
  # first occurrence of a topic, and after the latest
  topic_freqs <- topic_freqs %>%
    tidyr::complete(.data$topic_id,
                    timebin = seq.Date(min(.data$timebin),
                                       max(.data$timebin),
                                       by = timeBinUnit)) %>%
    #tidyr::complete(tidyr::crossing(topic_id, timebin = seq.Date(min(timebin),
    #                                                             max(timebin),
    #                                                             by = timeBinUnit))) %>%
    tidyr::replace_na(list(topicshare = 0,
                           mean_gamma = 0,
                           median_gamma = 0)) %>%
    dplyr::group_by(.data$topic_id) %>%
    dplyr::mutate(n_docs_topic = max(.data$n_docs_topic, na.rm = TRUE),
                  n_topic_timebins = max(.data$n_topic_timebins, na.rm = TRUE),
                  first_occur = min(.data$first_occur, na.rm = TRUE),
                  latest_occur = max(.data$latest_occur, na.rm = TRUE)) %>%
    dplyr::arrange(.data$timebin) %>%
    dplyr::mutate(topicshare_cumsum = cumsum(.data$topicshare)) %>%
    dplyr::filter(.data$topicshare_cumsum > 0) %>%
    dplyr::filter(!(.data$topicshare == 0 & .data$topicshare_cumsum == sum(.data$topicshare))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$topicshare_cumsum)

  return (topic_freqs)
}




#' Select top topics by document counts or temporal trend metric
#'
#' \code{select_top_topics} allows to select a specified number of top topics
#' based on miscellaneous properties of the topic frequencies. This method is
#' typically used to select a topic frequency time series for plotting and
#' exploratory analysis. See the details of the function arguments for selection
#' options.
#'
#' @param topicFrequencies a dataframe of \emph{topic} frequencies as returned
#'   by \code{\link{topic_frequencies}}
#'
#' @param topN the number of returned top topics meeting the selection criteria
#'   in \code{selectBy}
#'
#' @param selectBy the selection approach which determines the metric by which
#'   \code{topic_id}s will be sorted to select the \code{topN} terms. Currently,
#'   the following options are supported: \describe{
#'   \item{most_frequent}{\strong{the default}, select terms based on the total
#'   number of documents in which the topic occurs (\strong{NOTE}, that the
#'   document count depends on the minimum topic likelihood \code{minGamma} that
#'   was specified when obtaining the topic frequencies.)}
#'   \item{trending_up}{select topics with largest upwards trend; internally
#'   this is measured by the slope of a simple linear regression fit to a
#'   \code{topic_id}'s frequency series.} \item{trending_down}{select topics
#'   with largest downward trend; internally this is measured by the slope of a
#'   simple linear regression fit to a \code{topic_id}'s frequency series.}
#'   \item{trending}{select topics with either largest upward or downward trend;
#'   internally this is measured by the absolute value of the slope of a simple
#'   linear regression fit to a \code{topic_id}s frequency series.}
#'   \item{most_volatile}{select topics with the largest change throughout the
#'   covered time period; internally this is measured by the residual standard
#'   deviation of the linear model fit to a \code{topic_id}'s time frequency
#'   series.} }
#'
#' @return a dataframe specifying topic metrics employed for selecting top
#'   \code{topic}s, where: \describe{ \item{topic_id}{a unique topic identifier}
#'   \item{n_doc_topics}{the total number of documents in a dataset in which a
#'   topic with \code{topic_id} occurs} \item{slope}{the slope coefficient of a
#'   linear model fit to this \code{topic_id}'s time frequency series}
#'   \item{volatility}{the residual standard deviation of a linear model fit to
#'   this \code{topic_id}'s time frequency series} \item{trend}{a categorisation
#'   of the topic frequency trend} }
#'
#' @export
#'
select_top_topics <- function(topicFrequencies, topN = 25,
                              selectBy = "most_frequent") {

  # check selection options
  validSelectByOptions <- c("most_frequent", "trending", "trending_up",
                            "trending_down", "most_volatile")

  if (!(selectBy %in% validSelectByOptions)) {
    stop("'", selectBy, "' is not a valid option for selectBy. ",
         "Use one of: ", paste(validSelectByOptions, collapse = ", "))
  }

  # the number of topics are unlikely to be very large, but for optimal
  # efficiency we split the selection process into multiple steps to reduce
  # the number of topics for which we have to fit a regression
  top_n_topics <- topicFrequencies %>%
    dplyr::select(.data$topic_id, .data$n_docs_topic) %>%
    dplyr::distinct()

  # for efficiency we therefore handle "most_frequent" as a special case
  if(selectBy == "most_frequent") {
    top_n_topics <- top_n_topics %>%
      dplyr::arrange(-.data$n_docs_topic) %>%
      dplyr::slice(1:topN)
  }

  # for all other selection options and the final data we need the trend model
  topic_trends_lm <- topicFrequencies %>%
    dplyr::filter(.data$topic_id %in% top_n_topics$topic_id) %>%
    .topic_trends()

  # we select according to the chosen metric (specified in 'selectBy')
  top_n_topics <- top_n_topics %>%
    merge(topic_trends_lm, by = "topic_id")

  if(selectBy == "most_volatile") {
    top_n_topics <- dplyr::arrange(top_n_topics, -.data$volatility)
  } else if(selectBy == "trending") {
    top_n_topics <- dplyr::arrange(top_n_topics, -abs(.data$slope))
  } else if(selectBy == "trending_up") {
    top_n_topics <- dplyr::arrange(top_n_topics, -.data$slope)
  } else if(selectBy == "trending_down") {
    top_n_topics <- dplyr::arrange(top_n_topics, .data$slope)
  }

  top_n_topics <- dplyr::slice(top_n_topics, 1:topN)

  return(top_n_topics)
}





#' Fit a linear regression to time frequency series of each unique topic
#'
#' \code{.topic_trends} takes a dataframe with a topic frequency time series as
#' returned by the \code{\link{topic_frequencies}} function and fits a linear
#' regression for each \code{topic} (identified by \code{topic_id}).
#'
#' The primary purpose of this function is to provide a simple approach to
#' classify \code{topic}s by a general trend, which can be used for
#' visualization, filtering and exploratory analysis. Internally, the slope and
#' intercept of the fitted linear model are use to categorize a basic trend for
#' the topic frequencies.
#'
#' @param topicFrequencies a dataframe of \code{topic} frequencies as returned
#'   by \code{\link{topic_frequencies}}
#'
#' @param trendThreshold a double used to categorise trends, default is
#'   \code{0.0005}; if the intercept of the linear model of topic frequencies
#'   falls within a range of \code{+/-trendThreshold} of the mean topic share,
#'   the trend is categorized as \emph{"constant"}
#'
#' @return a dataframe specifying trend metrics for each \code{topic}, where:
#'   \describe{ \item{topic_id}{a topic identifier} \item{slope}{the slope
#'   coefficient of the linear model fit to this \code{topic}'s time frequency
#'   series} \item{volatility}{the residual standard deviation of the linear
#'   model fit to this \code{topic}'s time frequency series, which is used as a
#'   basic measure of volatility of topic frequencies} \item{trend}{a
#'   categorisation of the topic frequency trend, negative slopes with an
#'   intercept \code{> mean topic share + trendThreshold} are interpreted as
#'   \emph{decreasing}, positive slopes with an intercept \code{< mean topic
#'   share - trendThreshold} as \emph{increasing}, all others as
#'   \emph{constant}} }
#'
#' @keywords internal
#'
.topic_trends <- function(topicFrequencies, trendThreshold = 0.0005) {

  # index the timebin for fitting the lm; in order to get interpretable
  # intercepts when categorizing trends
  topicFrequencies <- topicFrequencies %>%
    dplyr::mutate(binindex = dplyr::dense_rank(.data$timebin) - 1)

  # linear model fit for each term
  lm_topics <- lapply(split(topicFrequencies, topicFrequencies$topic_id),
                      stats::lm,
                      formula = topicshare ~ binindex)

  topic_slope <- as.data.frame(t(sapply(lm_topics, stats::coefficients)))
  #topic_slope[1] <- NULL
  colnames(topic_slope) <- c("intercept", "slope")

  # use residual standard deviation as a basic measure of volatility
  topic_sigma <- data.frame(volatility = sapply(lm_topics, stats::sigma))

  # merge and categorise according to a threshhold/band
  topic_lm_trends <- merge(topic_slope, topic_sigma, by=0)
  colnames(topic_lm_trends)[1] <- c("topic_id")

  topic_lm_trends <- topicFrequencies %>%
    dplyr::group_by(.data$topic_id) %>%
    dplyr::summarize(mean_topicshare = mean(.data$topicshare)) %>%
    dplyr::ungroup() %>%
    merge(topic_lm_trends, by = "topic_id") %>%
    dplyr::mutate(trend = dplyr::case_when(
      .data$mean_topicshare - .data$intercept > trendThreshold ~ "increasing",
      .data$mean_topicshare - .data$intercept < -trendThreshold ~ "decreasing",
      TRUE ~ "constant")) %>%
    dplyr::select(-.data$mean_topicshare)

  return(topic_lm_trends)
}
