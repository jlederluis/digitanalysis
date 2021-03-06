% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unpack_round_numbers_test.R
\name{unpack_round_numbers_test}
\alias{unpack_round_numbers_test}
\title{Performs unpack rounded number test by performing all-digit place two-way chi square test vs Benford’s Law.
A wrapper function for \code{all_digit_test}.}
\usage{
unpack_round_numbers_test(
  digitdata,
  rounding_split_column,
  analysis_columns = "all",
  digit_places = "all",
  break_out = NA,
  break_out_grouping = NA,
  category = NA,
  category_grouping = NA,
  distribution = "Benford",
  contingency_table = NA,
  plot = TRUE,
  omit_05 = NA,
  skip_first_digit = FALSE,
  skip_last_digit = FALSE,
  suppress_low_N = FALSE,
  suppress_first_division_plots = FALSE,
  suppress_second_division_plots = TRUE
)
}
\arguments{
\item{digitdata}{A object of class \code{DigitAnalysis}.}

\item{rounding_split_column}{The data column (numeric!) to split rounded and unrounded digits upon to perform unpacking rounding test.}

\item{analysis_columns}{The names of numeric columns of data to be analyzed. Default can be 'all', where using all data columns in \code{numbers} df in \code{digitdata};
an array of column names, as characters; a single column name, as character.}

\item{digit_places}{The indexes of left-aligned digit places to analyze. There are three options:
\itemize{
  \item 'all': analzye all digits. Set as default.
  \item An numeric array: Perform multiple digit test on the digit places desired to analyze.
  \item A number: Perform single digit test on the digit place desired to analyze. If last digit test is desired, pass in -1 or c(-1).
}}

\item{break_out}{\itemize{
  \item The data column (non-numeric!) to split up the dataset based on different categories in the column if specified as an character.
  \item The first division (usually x-axis) shown in plots.
  \item Default to NA.
}}

\item{break_out_grouping}{A list of arrays, or defaulted to NA. Only effective if \code{break_out} is not NA.
\itemize{
  \item Each the names of the elements in the list is the break_out name
  \item Each array contains the values belonging to that break_out
  \item If it is remain as NA as default, while \code{break_out} is not NA, then \code{break_out_grouping} will default to every individual item in
  \code{break_out} will be in a separate group.
}}

\item{category}{The column for splitting the data into sectors for separate analysis. The second division (usually variables) shown in plots.}

\item{category_grouping}{A list of arrays, or defaulted to NA. Only effective if \code{category} is not NA.
\itemize{
  \item Each the names of the elements in the list is the category name
  \item Each array contains the values belonging to that category
  \item If it is remain as NA as default, while \code{category} is not NA, then \code{category_grouping} will default to every individual item in
  \code{category} will be in a separate group.
  \item e.g. \code{category_grouping = list(group_1=c(category_1, category_2, ...), group_2=c(category_10, ...), group_3=c(...))}
}}

\item{distribution}{'Benford' or 'Uniform'. Case insensitive. Specifies the distribution the chi square test is testing against. Default to 'Benford'.}

\item{contingency_table}{The user-input probability table of arbitrary distribution. Overwrites \code{distribution} if not NA.
Must be a dataframe of the form as \code{benford_table}. Defaulted to NA.
\itemize{
  \item Check out \code{load(file = "data/benford_table.RData")} to see the format of \code{benford_table}
}}

\item{plot}{TRUE or FALSE or 'Save': If TRUE, display the plots and return them. If 'Save', return the plots but suppress display. If FALSE, no plot is produced. Default to TRUE.}

\item{omit_05}{Whether to omit 0 or both 0 and 5. If omit both 0 and 5, pass in c(0,5) or c(5,0); if omit only 0 pass in 0 or c(0); if omit neither, pass in NA. Default to NA.}

\item{skip_first_digit}{TRUE or FALSE: If TRUE, skip first digit place before analysis. Default to FALSE.}

\item{skip_last_digit}{TRUE or FALSE: If TRUE, skip last digit place before analysis, since we don't want tests to overlap. Default to FALSE.
\code{skip_last_digit} should overwrite \code{digit_places} and \code{skip_first_digits}.}

\item{suppress_low_N}{TRUE or FALSE: If TRUE, suppress columns in expected table
if at least one cell in that column has expected value < 5. Default to FALSE.}

\item{suppress_first_division_plots}{TRUE or FALSE: If TRUE, suppress the display of all plots on first and second division.
If TRUE, \code{suppress_second_division_plots} will also be set to TRUE.}

\item{suppress_second_division_plots}{TRUE or FALSE: If TRUE, suppress the display of all plots on second division.}
}
\value{
\itemize{
  \item A list of p-values for round and unround data break by \code{break_out} and \code{category} if specified
  \item A list of sample sizes for  round and unround data break by \code{break_out} and \code{category} if specified
  \item A list of merged plots, rounded data plots, and un rounded data plots break by \code{break_out} and \code{category} if specified
  iff \code{plot = TRUE or 'Save'}
}
}
\description{
Performs unpack rounded number test by performing all-digit place two-way chi square test vs Benford’s Law.
A wrapper function for \code{all_digit_test}.
}
\examples{
unpack_round_numbers_test(digitdata, rounding_split_column='col_name', analysis_columns=c('X', 'Y'))
unpack_round_numbers_test(digitdata, rounding_split_column='col_name', digit_places=c(1,2,3), break_out='A', category='Y')
unpack_round_numbers_test(digitdata, rounding_split_column='col_name', break_out='A', omit_05=c(0,5), suppress_low_N=TRUE)
}
