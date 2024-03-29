% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topic-analysis.R
\name{.topic_trends}
\alias{.topic_trends}
\title{Fit a linear regression to time frequency series of each unique topic}
\usage{
.topic_trends(topicFrequencies, trendThreshold = 5e-04)
}
\arguments{
\item{topicFrequencies}{a dataframe of \code{topic} frequencies as returned
by \code{\link{topic_frequencies}}}

\item{trendThreshold}{a double used to categorise trends, default is
\code{0.0005}; if the intercept of the linear model of topic frequencies
falls within a range of \code{+/-trendThreshold} of the mean topic share,
the trend is categorized as \emph{"constant"}}
}
\value{
a dataframe specifying trend metrics for each \code{topic}, where:
  \describe{ \item{topic_id}{a topic identifier} \item{slope}{the slope
  coefficient of the linear model fit to this \code{topic}'s time frequency
  series} \item{volatility}{the residual standard deviation of the linear
  model fit to this \code{topic}'s time frequency series, which is used as a
  basic measure of volatility of topic frequencies} \item{trend}{a
  categorisation of the topic frequency trend, negative slopes with an
  intercept \code{> mean topic share + trendThreshold} are interpreted as
  \emph{decreasing}, positive slopes with an intercept \code{< mean topic
  share - trendThreshold} as \emph{increasing}, all others as
  \emph{constant}} }
}
\description{
\code{.topic_trends} takes a dataframe with a topic frequency time series as
returned by the \code{\link{topic_frequencies}} function and fits a linear
regression for each \code{topic} (identified by \code{topic_id}).
}
\details{
The primary purpose of this function is to provide a simple approach to
classify \code{topic}s by a general trend, which can be used for
visualization, filtering and exploratory analysis. Internally, the slope and
intercept of the fitted linear model are use to categorize a basic trend for
the topic frequencies.
}
\keyword{internal}
