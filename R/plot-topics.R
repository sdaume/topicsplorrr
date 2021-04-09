#' Plot individual term frequencies by date in a faceted plot
#'
#' \code{plot_term_frequencies} plots time series of term shares for a selection
#' of terms in a faceted plot. Each term is displayed in a single subplot and
#' each time series is overlayed with a linear trendline.
#'
#' This function merges the computation of term frequencies
#' (\code{\link{term_frequencies}}), creation of suitable labels
#' (\code{\link{terms_tokens_map}}) and the selection of terms by
#' msicalleneous criteria (\code{\link{select_top_terms}}) into one step.
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
#' @param topN the number of displayed top terms meeting the selection criteria
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
#' @param verboseLabels a Boolean indicating if a single terms should be used as
#'   labels for subplots or if multiple term instances (i.e. variations of the
#'   term found in the original text source) should be used as labels. The
#'   \strong{default is \code{FALSE}}.
#'
#' @param nCols the number of columns along which term subplots should be layed
#'   out.
#'
#' @family visualizations
#'
#' @export
#'
plot_term_frequencies <- function(termsByDate, timeBinUnit = "week",
                                  minTermTimeBins = 0.5, minTermOccurences = 10,
                                  topN = 25, selectTerms = NULL,
                                  selectTermsBy = "most_frequent",
                                  verboseLabels = FALSE, nCols = 5) {

  # candidates for additional arguments
  #sortBy = "trend" #we use the 'slope' of fitted linear regression as default

  term_freqs <- term_frequencies(termsByDate, timeBinUnit = timeBinUnit,
                                 minTermTimeBins = minTermTimeBins,
                                 minTermOccurences = minTermOccurences)

  terms_to_tokens <- terms_tokens_map(termsByDate = termsByDate)

  top_n_term_selection <- select_top_terms(term_freqs, topN = topN,
                                           selectTerms = selectTerms,
                                           selectBy = selectTermsBy)

  # here we merge the frequencies with the term-to-token map (used for
  # labelling) and we change the ordering of the terms in order to force
  # plotting in the facet plot according to the default sort order (i.e 'slope')
  terms_to_plot <- top_n_term_selection %>%
    dplyr::select(-.data$n_term_total) %>%
    merge(term_freqs, by = "term") %>%
    merge(terms_to_tokens, by = "term") %>%
    dplyr::arrange(.data$slope) %>%
    dplyr::mutate(term = factor(.data$term, levels = unique(.data$term)))

  # some tweaking of the axis limits and breaks for the plot (adding some spacing)
  x_scale_min <- min(terms_to_plot$timebin) - 7
  x_scale_max <- max(terms_to_plot$timebin) + 7

  # we want a sparsely labelled y-axis, two labels to be precise: 0 and the
  # multiple of 0.05 (i.e. 5%) that is closest to the global max of
  # term_share_per_timebin; alternatively a suitable value below 0.05
  y_scale_percent_steps <- 5
  y_scale_max_break <- ((max(terms_to_plot$term_share_per_timebin) * 100) %/%
                          y_scale_percent_steps) * y_scale_percent_steps / 100

  if (y_scale_max_break == 0) {
    #y_scale_max_break <- y_scale_percent_steps / 100
    y_scale_max_break <- (floor(max(terms_to_plot$term_share_per_timebin) * 100)) / 100
  }

  # we will us the max of the y_scale to position the term labels
  y_scale_max <- max(terms_to_plot$term_share_per_timebin)

  # set the labels used for each plot in the faceted plot, since we want to
  # drop the default strip.label boxes in a facet plot
  if(verboseLabels) {
    terms_to_plot <- terms_to_plot %>%
      dplyr::rowwise() %>%
      dplyr::mutate(term_label = dplyr::if_else(length(.data$term_instances) > 2,
                                                paste(paste(utils::head(.data$term_instances,2),
                                                            collapse = "\n"),
                                                      "..."),
                                                paste(utils::head(.data$term_instances,2),
                                                      collapse = "\n")))
  } else {
    terms_to_plot <- terms_to_plot %>%
      dplyr::mutate(term_label = .data$term)
  }

  ggplot2::ggplot(terms_to_plot, ggplot2::aes(x = timebin, y = term_share_per_timebin, group = term)) +
    ggplot2::geom_line(show.legend = FALSE, colour = "#bdbdbd") +
    ggplot2::geom_smooth(method = stats::lm, se = FALSE, formula = y ~ x,
                         ggplot2::aes(colour = trend),
                         show.legend = FALSE) +
    ggplot2::scale_y_continuous(limits = c(0, NA),
                                expand = c(0, 0),
                                breaks = c(0, y_scale_max_break),
                                labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_x_date(limits = c(x_scale_min, x_scale_max),
                          expand = c(0, 0),
                          labels = function(x) .custom_timeseries_month_label(x)) +
    ggplot2::geom_hline(yintercept = 0, color = "#969696") + # redraw x-axis line
    ggplot2::geom_text(x = min(terms_to_plot$timebin),
                       y = y_scale_max * 0.9,
                       vjust = 1, hjust = 0,
                       ggplot2::aes(label = term_label, colour = trend),
                       family = "Share Tech",
                       show.legend = FALSE, check_overlap = TRUE) +
    ggplot2::scale_color_manual(values = .term_trend_palette()) +
    ggplot2::facet_wrap(~term, ncol = nCols) +
    #xlab("") +
    #ylab("") +
    #ggtitle("TBD") +
    theme_stwd_base(baseSize = 10) +
    theme_fte_fonts(sizeAxisText = 10) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "#ffffff"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank())
}
