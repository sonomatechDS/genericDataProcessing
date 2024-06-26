% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/master_combination_list.R
\name{master_combination_list}
\alias{master_combination_list}
\title{Creates df with all combinations of variables}
\usage{
master_combination_list(data, ...)
}
\arguments{
\item{data}{Dataframe with all variables to be in be included in master list,
specified in the ... argument.}

\item{...}{Columns to group by (unquoted)}
}
\value{
data.frame
}
\description{
Specify dataframe/type, variables to include in combinations list.
Preset lists of variables are available to pass into 'combo'.
Either 'cols' or a valid 'combo' must be specified.
}
\details{
\strong{ASSUMES THAT DF's ARE PROCESSED and all variables are "cleaned"}

All DF's requested must contain set of cols.
}
\examples{
master_combination_list(mtcars, am, gear, carb)
}
