% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repeat_test.R
\name{repeat_test}
\alias{repeat_test}
\title{Performs repeat test across \code{break_out} category.}
\usage{
repeat_test(
  digitdata,
  break_out,
  data_column = NA,
  duplicate_matching_cols = "all",
  break_out_grouping = NA,
  rounding_patterns_to_omit = NA,
  plot = TRUE
)
}
\arguments{
\item{digitdata}{A object of class \code{DigitAnalysis}.}

\item{break_out}{\itemize{
  \item The data column (non-numeric!) to split up the dataset based on different categories in the column if specified as an character.
  \item The first division (usually x-axis) shown in plots.
}}

\item{data_column}{The data column to omit rounding numbers upon. Must be a single column.}

\item{duplicate_matching_cols}{An array of names of data columns two rows need to match exactly in order to be defined as a repeat.
Must include \code{data_column}. Default to 'all', meaning matching all columns in 'number' slot of \code{digitdata}.}

\item{break_out_grouping}{A list of arrays, or defaulted to NA. Only effective if \code{break_out} is not NA.
\itemize{
  \item Each the names of the elements in the list is the break_out name
  \item Each array contains the values belonging to that break_out
  \item If it is remain as NA as default, while \code{break_out} is not NA, then \code{break_out_grouping} will default to every individual item in
  \code{break_out} will be in a separate group.
}}

\item{rounding_patterns_to_omit}{The patterns to be counted as rounding digits to skip. Defaulted to NA.
\itemize{
  \item An array of characters such as c('0','00','000','5','50','500', '75', '25').
  \item \code{n_zeros_pattern} might be helpful for generating strings of 0s.
}}

\item{plot}{TRUE or FALSE or 'Save': If TRUE, display the plots and return them. If 'Save', return the plots but suppress display. If FALSE, no plot is produced. Default to TRUE.}
}
\value{
\itemize{
  \item A table of p values of t test for repeat test on each category
  \item A table of percent repeated numbers for repeat test on each category ordered by decreasing repeats percentage
  \item A table of sample sizes for repeat test on each category
  \item Plots for each category if \code{plot = TRUE or 'Save'}
  \item If NaN is in returned table, it means that there are no occurances of the data in that category --> 0/0 in percentage
}
}
\description{
Performs repeat test across \code{break_out} category.
}
\examples{
repeat_test(digitdata)
repeat_test(digitdata, duplicate_matching_cols=c('col_name1, col_name2'))
repeat_test(digitdata, duplicate_matching_cols=c('col_name1, col_name2'), break_out='col_name')
}
