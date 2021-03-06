% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_input_functions.R
\docType{class}
\name{DigitAnalysis-class}
\alias{DigitAnalysis-class}
\alias{DigitAnalysis}
\title{The class object for DigitAnalysis package}
\description{
The class object for DigitAnalysis package
}
\section{Slots}{

\describe{
\item{\code{raw}}{The raw dataframe passed in with a filepath or a dataframe without without NA columns}

\item{\code{cleaned}}{The cleaned dataframe with numeric data columns truncated and turned into integers if it is previous characters}

\item{\code{numbers}}{The numeric columns analyzing}

\item{\code{left_aligned}}{The left-aligned digits of each data column to be analyzed in \code{numbers}. i.e. a column is 'X'; first digit will be at column '1st digit' + 'X'.}

\item{\code{right_aligned}}{The right-aligned digits of each data column to be analyzed in \code{numbers}. i.e. a column is 'X'; first digit will be at column '1s' + 'X'.}

\item{\code{left_aligned_column_names}}{The naming method for left-aligned digit places in \code{left_aligned}}

\item{\code{right_aligned_column_names}}{The naming method for right-aligned digit places in \code{right_aligned}}

\item{\code{max}}{The maximum length of the numbers being analyzed in \code{all_digit_test}. Default to 0, not relevant outside of \code{all_digit_test}.}
}}

