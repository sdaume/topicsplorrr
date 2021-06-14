# Project: "Create a sample topic model for examples in vignettes"
#
# Script purpose: This fits one set of topic models with different topic numbers
#                 (K) and evaluates with regard to four metrics (heldout,
#                 residuals, semantic coherence and exclusivity).
#                 The heldouts, models and metrics are saved in approriately
#                 named '.Rdata' files indicating the topic modelling variation.
#
# Stefan Daume - 14.06.2021 (initial version)

library(dplyr)
library(stm)
library(quanteda)
library(topicsplorrr)


###############################################################################################
# CREATE A REFERENCE TOPIC MODEL AS PACKAGE DATA
###############################################################################################

# This code section allows to create an STM topic model which can be used as a
# reference point and is the basis of all examples included in the vignettes of
# this package. It is based on the quanteda corpus of the inaugural presidential
# speeches

# use quanteda to create the "original" document from a corpus
inaugural_speeches <- quanteda::convert(quanteda::data_corpus_inaugural,
                                        to = "data.frame") %>%
  mutate(Year = lubridate::as_date(paste(Year, "-01-20", sep = "")))

# prep the corpus using preprint abstracts
sample_dfm <- terms_dfm(textData = inaugural_speeches,
                        textColumn = "text",
                        documentIdColumn = "doc_id",
                        removeStopwords = TRUE,
                        removeNumbers = TRUE,
                        wordStemming = FALSE)

# run the topic modeller
sample_topics <- stm(sample_dfm, K = 25,
                     verbose = TRUE, seed = 8868467)


# extract and map the topic probabilities for all docs and topics
sample_topic_doc_probs <- topics_by_doc_date(sample_topics,
                                             termsDfm = sample_dfm,
                                             textData = inaugural_speeches,
                                             documentIdColumn = "doc_id",
                                             dateColumn = "Year")

# create the package datasets
usethis::use_data(sample_dfm, overwrite = FALSE)

usethis::use_data(sample_topics, overwrite = FALSE)

usethis::use_data(sample_topic_doc_probs, overwrite = FALSE)
