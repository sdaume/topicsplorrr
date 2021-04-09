#' Split a text source into tokens and terms by date of occurrence
#'
#' These functions transform a text source into a dataframe of individual terms
#' and tokens with an occurrence date. These terms/tokens can be extracted as
#' \emph{ngrams} of specified length. \code{terms_by_date} is wrapper around the
#' function for specific types of \emph{ngrams}.
#'
#' Text input (\code{textColumn}) is split with a \emph{word tokenizer}, default
#' stopwords (see \code{\link[tidytext:stop_words]{tidytext}}) are removed and
#' tokens are further processed and filtered according to the function's
#' options. A \emph{term} is the character sequence obtained after all NLP
#' processing options this function offers have been applied, most importantly
#' \emph{stemming}, here the Porter stemmer from the
#' \code{\link[SnowballC:wordStem]{SnowballC package}} is applied.
#'
#' @param textData a dataframe containing the text to be processed
#'
#' @param textColumn a character string specifying the column name in
#'   \code{textData} containing the text to be processed
#'
#' @param dateColumn a character string specifying the column name in
#'   \code{textData} specifying a publication date for the text in
#'   \code{textColumn}
#'
#' @param removeNumbers a Boolean indicating whether numbers should be removed
#'   from the result; default is \strong{TRUE}.
#'
#' @param wordStemming a Boolean indicating whether words in the text should be
#'   reduced to the word stem; default is \strong{TRUE}.
#'
#' @param customStopwords a character vector specifying additional stopwords
#'   that should be removed from the result
#'
#' @param tokenType the length of the consecutive token sequence extracted,
#'   currently only \code{bigram} (two word sequence) and \code{unigram} (single
#'   words) are supported, with \strong{\code{unigram} as default}
#'
#' @return a dataframe with three columns listing all individual term
#'   occurrences in the provided text source, where \code{occur} is the
#'   publication date associated with an original \code{token}, which has been
#'   processed/reduced to \code{term}; if no stemming has been applied the term
#'   and token in the result are identical
#'
#' @name extract-term-ngrams
NULL


#' @export
#' @rdname extract-term-ngrams
#'
terms_by_date <- function(textData, textColumn, dateColumn,
                          removeNumbers = TRUE, wordStemming = TRUE,
                          customStopwords = NULL,
                          tokenType = "unigram") {

  if (tokenType == "unigram") {
    terms_over_time <- unigrams_by_date(textData, textColumn, dateColumn,
                                        removeNumbers, wordStemming,
                                        customStopwords)
  } else if (tokenType == "bigram") {
    terms_over_time <- bigrams_by_date(textData, textColumn, dateColumn,
                                       removeNumbers, wordStemming,
                                       customStopwords)
  } else {
    warning("'", tokenType, "' is not a supported tokenization method.")
    terms_over_time <- NULL
  }

  return(terms_over_time)
}



#' @export
#' @rdname extract-term-ngrams
#'
unigrams_by_date <- function(textData, textColumn, dateColumn,
                             removeNumbers = TRUE, wordStemming = TRUE,
                             customStopwords = NULL) {

  # 'stemLanguage' can be turned into a function argument if we want to use this
  # for texts in other languages
  stemLanguage <- "en"

  textObj <- dplyr::sym(textColumn)
  dateObj <- dplyr::sym(dateColumn)

  terms_over_time <- textData %>%
    dplyr::select(!!textObj, !!dateObj) %>%
    tidytext::unnest_tokens(output = "token",
                            input = !!textObj,
                            drop = TRUE) %>%
    dplyr::anti_join(tidytext::stop_words, by = c("token" = "word"))

  if (!is.null(customStopwords) && length(customStopwords) > 0) {
    terms_over_time <- terms_over_time %>%
      dplyr::filter(!(.data$token %in% customStopwords))
  }

  if(removeNumbers) {
    terms_over_time <- terms_over_time %>%
      dplyr::filter(!stringr::str_detect(.data$token,
                                         "^-?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?$"))
  }

  if(wordStemming) {
    terms_over_time <- terms_over_time %>%
      dplyr::mutate(term = SnowballC::wordStem(.data$token,
                                               language = stemLanguage))
  } else {
    terms_over_time <- terms_over_time %>%
      dplyr::mutate(term = .data$token)
  }

  terms_over_time <- terms_over_time %>%
    dplyr::rename(occur = !!dateObj)

  return(terms_over_time)
}



#' @importFrom rlang .data
#' @export
#' @rdname extract-term-ngrams
#'
bigrams_by_date <- function(textData, textColumn, dateColumn,
                            removeNumbers = TRUE, wordStemming = TRUE,
                            customStopwords = NULL) {

  # 'stemLanguage' can be turned into a function argument if we want to use this
  # for texts in other languages
  stemLanguage <- "en"

  textObj <- dplyr::sym(textColumn)
  dateObj <- dplyr::sym(dateColumn)

  terms_over_time <- textData %>%
    dplyr::select(!!textObj, !!dateObj) %>%
    tidytext::unnest_tokens(output = "ngram",
                            input = !!textObj,
                            token = "ngrams",
                            n = 2,
                            drop = TRUE) %>%
    tidyr::separate(.data$ngram, c("token1", "token2"), sep = " ") %>%
    dplyr::anti_join(tidytext::stop_words, by = c("token1" = "word")) %>%
    dplyr::anti_join(tidytext::stop_words, by = c("token2" = "word"))


  if(!is.null(customStopwords) && length(customStopwords) > 0) {
    terms_over_time <- terms_over_time %>%
      dplyr::filter(!(.data$token1 %in% customStopwords | .data$token2 %in% customStopwords))
  }


  if(removeNumbers) {
    terms_over_time <- terms_over_time %>%
      dplyr::filter(!stringr::str_detect(.data$token1,
                                         "^-?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?$")) %>%
      dplyr::filter(!stringr::str_detect(.data$token2,
                                         "^-?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?$"))
  }


  if(wordStemming) {
    terms_over_time <- terms_over_time %>%
      dplyr::mutate(term1 = SnowballC::wordStem(.data$token1, language="en"),
                    term2 = SnowballC::wordStem(.data$token2, language="en"))
  } else {
    terms_over_time <- terms_over_time %>%
      dplyr::mutate(term1 = .data$token1,
                    term2 = .data$token2)
  }

  terms_over_time <- terms_over_time %>%
    tidyr::unite("term", .data$term1, .data$term2, sep = " ") %>%
    tidyr::unite("token", .data$token1, .data$token2, sep = " ") %>%
    dplyr::rename(occur = !!dateObj)

  return(terms_over_time)
}





#' Create a document-feature-matrix from a text source
#'
#' \code{terms_dfm} takes a text source with text objects associated with unique
#' document identifiers and creates a document-feature-matrix, which can be used
#' as input for an \code{\link[stm:stm]{stm}} topic modeller.
#'
#' Text input (\code{textColumn}) is split with a \emph{word tokenizer} and
#' tokens are further processed and filtered according to the function's
#' options. Since the result is primarily intended as input for a topic
#' modeller, stopwords (see \code{\link[tidytext:stop_words]{tidytext}}) are
#' \strong{not} removed by default.
#'
#' @param textData a dataframe containing the text to be processed, with each
#'   row representing a distinct document
#'
#' @param textColumn the column name in \code{textData} containing the text to
#'   be processed
#'
#' @param documentIdColumn the column name in \code{textData} specifying a
#'   unique identifier for the document with the content given in
#'   \code{textColumn}
#'
#' @param removeStopwords a Boolean indicating whether standard stopwords (see
#'   \code{\link[tidytext:stop_words]{tidytext}}) should be removed from the
#'   result; default is \strong{FALSE}.
#'
#' @param removeNumbers a Boolean indicating whether numbers should be removed
#'   from the result; default is \strong{FALSE}. If \strong{TRUE}, a the Porter
#'   stemmer from the \code{\link[SnowballC:wordStem]{SnowballC package}} is
#'   applied.
#'
#' @param wordStemming a Boolean indicating whether words in the text should be
#'   reduced to the word stem; default is \strong{FALSE}.
#'
#' @param customStopwords a character vector specifying additional stopwords
#'   that should be removed from the result
#'
#' @return a \emph{document-feature-matrix} of type
#'   \code{\link[quanteda:dfm]{quanteda::dfm}} (similar to a
#'   document-term-matrix), where a \emph{document} is identified by the value
#'   in the \code{documentIdColumn} specified in the text source (i.e.
#'   \code{textData}), and a \emph{feature} or \emph{term} is a character
#'   sequence obtained after tokenization and all other NLP processing options
#'   have been applied to the text associated with a document.
#'
#' @export
#'
terms_dfm <- function(textData, textColumn, documentIdColumn,
                      removeStopwords = FALSE, removeNumbers = FALSE,
                      wordStemming = FALSE, customStopwords = NULL) {

  # We will use our custom approach (as in terms_by_date()) for text processing
  # and preparation and then map this to a document-feature-matrix (quanteda::dfm)
  # as input for topic modelling. It might be useful to use extended features of
  # a `dfm` later, thus by using `dfm` it will be easy to refactor this function
  # without breaking the rest of our topic modelling workflow.

  # 'stemLanguage' can be turned into a function argument if we want to use this
  # for texts in other languages
  stemLanguage <- "en"

  textObj <- dplyr::sym(textColumn)
  docIdObj <- dplyr::sym(documentIdColumn)

  terms_by_document <- textData %>%
    dplyr::select(!!textObj, !!docIdObj) %>%
    tidytext::unnest_tokens(output = "token",
                            input = !!textObj,
                            drop = TRUE)

  if(removeStopwords) {
    terms_by_document <- terms_by_document %>%
      dplyr::anti_join(tidytext::stop_words, by = c("token" = "word")) %>%
      dplyr::filter(!(.data$token %in% customStopwords))
  }

  if(removeNumbers) {
    terms_by_document <- terms_by_document %>%
      dplyr::filter(!stringr::str_detect(.data$token,
                                         "^-?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?$"))
  }

  if(wordStemming) {
    terms_by_document <- terms_by_document %>%
      dplyr::mutate(term = SnowballC::wordStem(.data$token, language = stemLanguage))
  } else {
    terms_by_document <- terms_by_document %>%
      dplyr::mutate(term = .data$token)
  }

  terms_by_document <- terms_by_document %>%
    dplyr::count(!!docIdObj, .data$term, sort = TRUE) %>%
    tidytext::cast_dfm(!!docIdObj, .data$term, .data$n)

  return(terms_by_document)
}
