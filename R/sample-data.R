#' Document-feature-matrix of the inaugural presidential speeches
#'
#' This package data provides the document-feature-matrix (DFM) used to fit the
#' \code{\link[stm:stm]{stm}} topic model \code{\link{sample_topics}}. The DFM
#' is based on the inaugural presidential speeches in the respective dataset
#' included in the \code{quanteda} package
#' (\code{\link{quanteda::data_corpus_inaugural}}), with stopwords and numbers
#' removed.
#'
#' This dataset is included for convenience and reproducibility.
#'
#' @format a \code{\link[quanteda]{quanteda}} dfm
#'
"sample_dfm"


#' A STM topic model of the 'covid19preprints' preprint abstracts
#'
#' This package dataset is a \code{\link[stm:stm]{stm}} topic model for the
#' \code{\link{sample_dfm}} with 25 topics; it was fit to the inaugural
#' presidential speeches in the respective dataset included in the
#' \code{quanteda} package (\code{\link{quanteda::data_corpus_inaugural}}).
#'
#' This dataset is included for convenience and reproducibility; depending on
#' the respective system configuration estimating a topic model can require a
#' long-running process.
#'
#' @format a \code{\link[stm:stm]{stm}} object
#'
"sample_topics"



#' Per-document topic probabilities for the `covid19preprints` topics
#'
#' This package data set summarizes the per document topic probabilities
#' according to the \code{\link[stm:stm]{stm}} topic model
#' \code{\link{sample_topics}} fit to the inaugural presidential speeches in the
#' respective dataset included in the \code{quanteda} package
#' (\code{\link{quanteda::data_corpus_inaugural}}).
#'
#' This dataset is included for convenience and reproducibility, most analysis
#' and visualization functions in this package rely on the per-document topic
#' probabilities.
#'
#' @format a dataframe with 4 variables: \describe{ \item{document}{a unique
#'   document identifier, here a combination of president and year of speech}
#'   \item{occur}{the (assumed) date of \code{document} (i.e. speech) as a Date
#'   object} \item{topic_id}{a topic ID as assigned in the applied topic model}
#'   \item{gamma}{the probability that the given \code{topic_id} explains the
#'   given \code{document}; in the analysis we interpret this as the topic's
#'   share in this document} }
#'
"sample_topic_doc_probs"
