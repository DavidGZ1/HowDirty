% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrange_contagroup_levels.R
\name{arrange_contagroup_levels}
\alias{arrange_contagroup_levels}
\title{Arrange contaminant group levels}
\usage{
arrange_contagroup_levels(df_conta, metric = "median")
}
\arguments{
\item{df_conta}{dataframe containing the contaminants.}
}
\value{
dataframe with the arranged contaminant group levels.
}
\description{
Arranges the contaminant group levels based on overall median abundance.
}
\examples{
arrange_contagroup_levels(df_conta, metric = "median")

}
