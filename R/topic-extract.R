#' Extract topic likelihoods per document for an STM topic model
#'
#' \code{topics_by_doc_date} retrieves the likelihoods that a document is
#' generated from a topic (\emph{gamma} in the passed topic STM model). Related
#' functions and examples in this package interpret this likelihood effectively
#' as a topic share.
#'
#' @param topicModel an \code{\link[stm:stm]{stm topic model}} fit to the
#'   document-feature-matrix provided in the \code{termsDfm} argument
#'
#' @param termsDfm a \emph{document-feature-matrix} of type
#'   \code{\link[quanteda:dfm]{quanteda::dfm}} that was used to fit the provided
#'   \code{topicModel} (see \code{\link{terms_dfm}}).
#'
#' @param textData a dataframe containing the original text source reduced to
#'   the document-feature-matrix provided in the \code{termsDfm} argument
#'
#' @param dateColumn the column name in \code{textData} containing a publication
#'   date associated with a document identified by a unique identifier in
#'   \code{documentIdColumn}
#'
#' @param documentIdColumn the column name in \code{textData} specifying a
#'   unique identifier for a document
#'
#' @return a dataframe with topic shares/likelihoods per document, where:
#'   \describe{\item{document}{a unique identifier for a document}
#'   \item{occur}{a publication date associated with \code{document}}
#'   \item{topic_id}{a numeric identifier of a topic} \item{gamma}{the
#'   likelihood that \code{document} is generated from the topic with
#'   \code{topic_id}; the analyses in this package interprete this likelihood as
#'   the share of this topic in this document.} }
#'
#' @importFrom rlang .data
#' @export
#'
topics_by_doc_date <- function(topicModel, termsDfm, textData,
                               documentIdColumn, dateColumn) {

  # the topic to document assignments with probabilities "gamma"
  topics_document_matrix <- tidytext::tidy(topicModel,
                                           matrix = "gamma",
                                           document_names = row.names(termsDfm)) %>%
    dplyr::rename(topic_id = .data$topic)

  # merge topics with dates (and potentially other info)
  #
  # here we choose to combine (by document Id) the topic shares with the
  # original textData in order to extract topic frequencies along a timescale.
  # `stm` offers an option to include time as a covariate, but it requires
  # continous variables (i.e. indexed days (1,2,3, ...)), that would then have
  # to be matched back to a date-time scale. The approach here is deemed
  # more flexible if we DON'T want to include time as covariate; furthermore,
  # we typically  want to be able to use variable time bin units in later
  # analysis steps

  dateObj <- dplyr::sym(dateColumn)

  topics_document_matrix <- topics_document_matrix %>%
    merge(textData, by.x = "document", by.y = documentIdColumn) %>%
    dplyr::select(.data$document, .data$topic_id, .data$gamma, !!dateObj) %>%
    dplyr::rename(occur = !!dateObj) %>%
    dplyr::mutate(topic_id = as.character(.data$topic_id)) %>%
    dplyr::select(.data$document, .data$occur, .data$topic_id, .data$gamma)

  return (topics_document_matrix)
}



#' Extract a mapping between topics and representative topic terms from an STM
#'
#' The primary purpose of \code{topics_terms_map} is to provide an association
#' between topics (by ID) and a representative set of words (terms) for a given
#' topic. These can be used to create labels for visualizations.
#'
#' By default the terms that are both most frequent and exclusive are used (see
#' \code{\link[stm:stm]{stm}} for details).
#'
#' @param topicModel an \code{\link[stm:stm]{stm topic model}}
#'
#' @param nLabelTerms the number of terms to be included as a label for a given
#'   topic; default is \code{7}.
#'
#' @param labelType the \code{stm} word weighting used to determine the words
#'   most representative of a topic; default is \emph{"frex"} (most frequent and
#'   exclusive terms). Other options are \emph{"prob"}, \emph{"lift"},
#'   \emph{"score"} (see \code{\link[stm:labelTopics]{stm::labelTopics}} for
#'   details)
#'
#' @return a dataframe with topics by ID and a selection of associated topic
#'   terms, where: \describe{ \item{topic_id}{a numeric identifier of a topic}
#'   \item{topic_label}{a character sequence of frequent and exclusive terms
#'   associated with a topic}}
#'
#' @importFrom rlang .data
#' @export
#'
topics_terms_map <- function(topicModel, nLabelTerms = 7, labelType = "frex") {

  # this is a basic initial implementation modelled after the equivalent
  # function terms_tokens_map(), thus allowing the same processing
  # workflow/pipeline; it could be extended offering options to select which
  # terms to use as topic labels and could also include the invidual term
  # likelihoods (similar to n_term_instances in terms_tokens_map()), if we
  # wanted to support more sophisticated labelling for visualizations

  stm_topic_labels <- NULL

  if (labelType == "frex") {
    stm_topic_labels <- stm::labelTopics(topicModel, n = nLabelTerms)$frex
  } else if (labelType == "prob") {
    stm_topic_labels <- stm::labelTopics(topicModel, n = nLabelTerms)$prob
  } else if (labelType == "lift") {
    stm_topic_labels <- stm::labelTopics(topicModel, n = nLabelTerms)$lift
  } else if (labelType == "score") {
    stm_topic_labels <- stm::labelTopics(topicModel, n = nLabelTerms)$score
  } else {
    warning(paste("'", labelType,
                  "' is not a valid label type. Using 'frex' instead.",
                  sep = ""))
    stm_topic_labels <- stm::labelTopics(topicModel, n = nLabelTerms)$frex
  }

  # get the topic labels
  topic_labels <- stm_topic_labels %>%
    data.frame() %>%
    tidyr::unite("topic_label", sep = " ") %>%
    dplyr::mutate(topic_label = strsplit(.data$topic_label, split = " ")) %>%
    tibble::rownames_to_column(var = "topic_id") %>%
    dplyr::select(.data$topic_id, .data$topic_label)

  return(topic_labels)
}
