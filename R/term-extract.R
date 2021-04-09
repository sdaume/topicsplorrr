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
#' @param textColumn the column name in \code{textData} containing the text to
#'   be processed
#'
#' @param dateColumn the column name in \code{textData} specifying a publication
#'   date for the text in \code{textColumn}
#'
#' @param tokenType the length of the consecutive token sequence extracted,
#'   currently only \code{bigram} (two word sequence) and \code{unigram} (single
#'   words) are supported, with \strong{\code{unigram} as default}
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
terms_by_date <- function(..., tokenType = "unigram") {

  if (tokenType == "unigram") {
    terms_over_time <- unigrams_by_date(...)
  } else if (tokenType == "bigram") {
    terms_over_time <- bigrams_by_date(...)
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
                             customStopwords = c()) {

  # 'stemLanguage' can be turned into a function argument if we want to use this
  # for texts in other languages
  stemLanguage <- "en"

  textObj <- dplyr::sym(textColumn)
  dateObj <- dplyr::sym(dateColumn)

  terms_over_time <- textData %>%
    dplyr::select(!!textObj, !!dateObj) %>%
    tidytext::unnest_tokens(output = token,
                            input = !!textObj,
                            drop = TRUE) %>%
    dplyr::anti_join(tidytext::stop_words, by = c("token" = "word")) %>%
    dplyr::filter(!(token %in% customStopwords)) %>%
    {if(removeNumbers)
      dplyr::filter(., !stringr::str_detect(token,
                                            "^-?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?$"))
      else .} %>%
    dplyr::mutate(term = token) %>%
    {if(wordStemming)
      dplyr::mutate(., term = SnowballC::wordStem(token, language = stemLanguage))
      else .} %>%
    dplyr::rename(occur = !!dateObj)

  return(terms_over_time)
}



#' @export
#' @rdname extract-term-ngrams
#'
bigrams_by_date <- function(textData, textColumn, dateColumn,
                            removeNumbers = TRUE, wordStemming = TRUE,
                            customStopwords = c()) {

  # 'stemLanguage' can be turned into a function argument if we want to use this
  # for texts in other languages
  stemLanguage <- "en"

  textObj <- dplyr::sym(textColumn)
  dateObj <- dplyr::sym(dateColumn)

  terms_over_time <- textData %>%
    dplyr::select(!!textObj, !!dateObj) %>%
    tidytext::unnest_tokens(output = ngram,
                            input = !!textObj,
                            token = "ngrams",
                            n = 2,
                            drop = TRUE) %>%
    tidyr::separate(ngram, c("token1", "token2"), sep = " ") %>%
    dplyr::anti_join(tidytext::stop_words, by = c("token1" = "word")) %>%
    dplyr::anti_join(tidytext::stop_words, by = c("token2" = "word")) %>%
    {if(length(customStopwords) > 0)
      dplyr::filter(., !(token1 %in% customStopwords | token2 %in% customStopwords))
      else .} %>%
    {if(removeNumbers)
      dplyr::filter(., !stringr::str_detect(token1,
                                            "^-?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?$")) %>%
        dplyr::filter(!stringr::str_detect(token2,
                                           "^-?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?$"))
      else .} %>%
    dplyr::mutate(term1 = token1,
                  term2 = token2) %>%
    {if(wordStemming)
      dplyr::mutate(.,
                    term1 = SnowballC::wordStem(token1, language="en"),
                    term2 = SnowballC::wordStem(token2, language="en"))
      else .} %>%
    tidyr::unite(term, term1, term2, sep = " ") %>%
    tidyr::unite(token, token1, token2, sep = " ") %>%
    dplyr::rename(occur = !!dateObj)

  return(terms_over_time)
}


