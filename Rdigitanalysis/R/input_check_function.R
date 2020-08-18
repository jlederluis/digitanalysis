############################################################
#Functions for digit analysis R package
###input check function in this file
#Wenjun Chang
#Summer 2020
############################################################

#' Asserts the input to user accessible functions in this package are of correct type and form.
#'
#' @param digitdata Mandatory option. Check if it is of class \code{DigitAnalysis}
#' @param contingency_table Defaulted to NA. Pass in to check if it is a valid input.
#' @param data_columns Defaulted to NA. Pass in to check if it is a valid input.
#' @param digit_places Defaulted to NA. Pass in to check if it is a valid input.
#' @param skip_first_digit Defaulted to NA. Pass in to check if it is a valid input.
#' @param omit_05 Defaulted to NA. Pass in to check if it is a valid input.
#' @param break_out Defaulted to NA. Pass in to check if it is a valid input.
#' @param distribution Defaulted to NA. Pass in to check if it is a valid input.
#' @param plot Defaulted to NA. Pass in to check if it is a valid input.
#' @param skip_last_digit Defaulted to NA. Pass in to check if it is a valid input.
#' @param unpacking_rounding_column Defaulted to NA. Pass in to check if it is a valid input.
#' @param min_length Defaulted to NA. Pass in to check if it is a valid input.
#' @param duplicate_matching_cols Defaulted to NA. Pass in to check if it is a valid input.
#' @param category Defaulted to NA. Pass in to check if it is a valid input.
#' @param category_grouping Defaulted to NA. Pass in to check if it is a valid input.
#' @param high Defaulted to NA. Pass in to check if it is a valid input.
#' @param max_length Defaulted to NA. Pass in to check if it is a valid input.
#' @param num_digits Defaulted to NA. Pass in to check if it is a valid input.
#' @param N Defaulted to NA. Pass in to check if it is a valid input.
#' @param standard_df Defaulted to NA. Pass in to check if it is a valid input.
#' @param suppress_low_N Defaulted to NA. Pass in to check if it is a valid input.
#' @param suppress_first_division_plots Defaulted to NA. Pass in to check if it is a valid input.
#' @param suppress_second_division_plots Defaulted to NA. Pass in to check if it is a valid input.
#'
#' @return Throw error with message if input is of incorrect type. Returns nothing.
input_check = function(digitdata, contingency_table=NA, data_columns=NA, digit_places=NA, skip_first_digit=NA, omit_05=NA, break_out=NA,
                       break_out_grouping=NA, distribution=NA, plot=NA, skip_last_digit=NA, unpacking_rounding_column=NA, min_length=NA,
                       duplicate_matching_cols=NA, category=NA, category_grouping=NA, high=NA, max_length=NA, num_digits=NA, N=NA,
                       standard_df=NA, rounding_patterns=NA, suppress_low_N=NA, suppress_first_division_plots=NA, suppress_second_division_plots=NA) {

  #digitdata must be of class DigitAnalysis
  if (class(digitdata)[1] != 'DigitAnalysis'){
    stop("digitdata must be an object in the class DigitAnalysis! data need to be preprocessed into the DigitAnalysis format, use function make_class()!")
  }

  #contingency_table must be a dataframe
  if (TRUE %in% (!(is.na(contingency_table)))){
    if (!(is.data.frame(contingency_table))){
      stop("contingency_table must be a dataframe!")
    }
  }

  #data_columns must be either 'all', a single column name, or an array of column names desired to analyze
  if (TRUE %in% (!(is.na(data_columns)))){
    if (data_columns != 'all'){
      if (!(is.character(data_columns))){
        stop("data_columns must be either 'all', a single column name, or an array of column names desired to analyze!")
      }
    }
  }

  #check digit_places
  if (TRUE %in% (!(is.na(digit_places)))){
    #digit_places must be either 'all', a single digit place integer, or an array of digit places desired to analyze
    if (digit_places != 'all'){
      if (!(is.numeric(digit_places))){
        stop("digit_places must be either 'all', a single digit place integer, or an array of digit places desired to analyze!")
      }
      #multiple digit places test cannot have -1 as part of the array
      if (length(digit_places) > 1){
        if (!is.na(match(-1, digit_places))){
          stop('multiple digits test cannot have last digit as part of the digit places input!')
        }
      }
    }
    #should not have 1 as part of digit_places if we are skipping first digit place using skip_first_digit
    if (TRUE %in% (!(is.na(skip_first_digit)))){
      if (skip_first_digit){
        if (!is.na(match(1, digit_places))){
          stop('digit_places and skip_first_digit contradicts, both looking and not looking at the first digit place!')
        }
      }
    }
  }

  #skip_first_digit must be a boolean: TRUE or FALSE
  if (TRUE %in% (!(is.na(skip_first_digit)))){
    if (!(is.logical(skip_first_digit))){
      stop('skip_first_digit must be a boolean: TRUE or FALSE!')
    }
  }

  #omit_05 must be either NA, 0, c(0,5), or c(5,0)
  if (TRUE %in% (!(is.na(omit_05)))){
    if (length(omit_05) == 1){
      #must be 0
      if (omit_05 != 0){
        stop('omit_05 must be either NA, 0, c(0,5), or c(5,0)')
      }
    }
    else if (length(omit_05) == 2){
      #0 and 5 should be the two elements
      if (length(omit_05[omit_05 != 5]) != 1){
        stop('omit_05 must be either NA, 0, c(0,5), or c(5,0)')
      }
      else if (omit_05[omit_05 != 5] != 0){
        stop('omit_05 must be either NA, 0, c(0,5), or c(5,0)')
      }
    }
    #3 elements does not make sense
    else {
      stop('omit_05 must be either NA, 0, c(0,5), or c(5,0)')
    }
  }

  #break_out must be a string specifying a column name in the data
  if (TRUE %in% (!(is.na(break_out)))){
    if (!(is.character(break_out))){
      stop('break_out must be a string specifying a column name in the data!')
    }
  }

  #break_out_grouping must be a list specifying the columns for grouping the first division of data to split upon
  if (TRUE %in% (!(is.na(break_out_grouping)))){
    if (!(is.list(break_out_grouping))){
      stop('break_out_grouping must be a list specifying the columns for grouping the first division of data to split upon!')
    }
  }

  #distribution must be either 'Benford' or 'Uniform'
  if (TRUE %in% (!(is.na(distribution)))){
    if (!(distribution %in% c('Benford', 'Uniform', 'benford', 'uniform'))){
      stop("distribution must be either 'Benford' or 'Uniform', case insensitive!")
    }
  }

  #plot must be a boolean: TRUE or FALSE
  if (TRUE %in% (!(is.na(plot)))){
    if (!(is.logical(plot))){
      stop('plot must be a boolean: TRUE or FALSE!')
    }
  }

  #skip_last_digit must be a boolean: TRUE or FALSE
  if (TRUE %in% (!(is.na(skip_last_digit)))){
    if (!(is.logical(skip_last_digit))){
      stop('skip_last_digit must be a boolean: TRUE or FALSE!')
    }
    #if doing last digit test, should not skip_last_digit
    if (TRUE %in% (!(is.na(digit_places)))){
      if (length(digit_places) == 1){
        if (digit_places == -1){
          if (skip_last_digit){
            stop('skip_last_digit while doing a last digit test: contradiction!')
          }
        }
      }
    }
  }

  #unpacking_rounding_column must be a string specifying a column name in the data
  if (TRUE %in% (!(is.na(unpacking_rounding_column)))){
    if (!(is.character(unpacking_rounding_column))){
      stop('unpacking_rounding_column must be a string specifying a column name in the data!')
    }
  }

  #min_length must be an integer >= 2 denoting the minimum length of the numbers to be analyzed in digit pair test
  if (TRUE %in% (!(is.na(min_length)))){
    if (length(min_length) != 1){
      stop("min_length must be an integer >= 2 denoting the minimum length of the numbers to be analyzed in digit pair test!")
    }
    else if (!(is.numeric(min_length))){
      stop("min_length must be an integer >= 2 denoting the minimum length of the numbers to be analyzed in digit pair test!")
    }
  }

  #duplicate_matching_cols must be a single column name or an array of column names to define a exact duplicate
  if (TRUE %in% (!(is.na(duplicate_matching_cols)))){
    if (duplicate_matching_cols[1] != 'all'){
      if (!(is.character(duplicate_matching_cols))){
        stop("duplicate_matching_cols must be a single column name or an array of column names to define a exact duplicate!")
      }
    }
  }

  #category must be a string specifying a column name in the data
  if (TRUE %in% (!(is.na(category)))){
    if (!(is.character(category))){
      stop('category must be a string specifying a column name in the data!')
    }
  }

  #category_grouping must be a list specifying the columns in each sector intended to split upon second division
  if (TRUE %in% (!(is.na(category_grouping)))){
    if (!(is.list(category_grouping))){
      stop('category_grouping must be a list specifying the columns in each sector intended to split upon second division!')
    }
  }

  #high must be a single digit place integer or an array of digit places specifying the digits that are classified as high digits
  if (TRUE %in% (!(is.na(high)))){
    if (!(is.numeric(high))){
      stop("high must be a single digit place integer or an array of digit places specifying the digits that are classified as high digits!")
    }
  }

  #max_length must be an integer denoting the maximum length of the numbers to be analyzed in padding test
  if (TRUE %in% (!(is.na(max_length)))){
    if (length(max_length) != 1){
      stop("max_length must be an integer denoting the maximum length of the numbers to be analyzed in padding test!")
    }
    else if (!(is.numeric(max_length))){
      stop("max_length must be an integer denoting the maximum length of the numbers to be analyzed in padding test!")
    }
  }

  #num_digits must be an integer denoting the number of digits from the right to be to be analyzed in padding test
  if (TRUE %in% (!(is.na(num_digits)))){
    if (length(num_digits) != 1){
      stop("num_digits must be an integer denoting the number of digits from the right to be to be analyzed in padding test!")
    }
    else if (!(is.numeric(num_digits))){
      stop("num_digits must be an integer denoting the number of digits from the right to be to be analyzed in padding test!")
    }
  }

  #N must be an integer denoting the number of datasets to be stimulated from Monte Carlo Process in padding test
  if (TRUE %in% (!(is.na(N)))){
    if (length(N) != 1){
      stop("N must be an integer denoting the number of datasets to be stimulated from Monte Carlo Process in padding test!")
    }
    else if (!(is.numeric(N))){
      stop("N must be an integer denoting the number of datasets to be stimulated from Monte Carlo Process in padding test!")
    }
  }
  if (TRUE %in% (!(is.na(standard_df)))){
    if (length(standard_df) != 1){
      stop("standard_df must be a boolean denoting whether to use standard method or digit analysis method for calculating degrees of freedom!")
    }
    else if (!(is.logical(standard_df))){
      stop("standard_df must be a boolean denoting whether to use standard method or digit analysis method for calculating degrees of freedom!")
    }
  }
  if (TRUE %in% (!(is.na(rounding_patterns)))){
    if (!(is.character(rounding_patterns))){
      stop("rounding_patterns must be an array of characters denoting the endstring patterns that should be considered as rounding!")
    }
  }
  if (TRUE %in% (!(is.na(suppress_low_N)))){
    if (!(is.logical(suppress_low_N))){
      stop("suppress_low_N must be a boolean denoting whether to suppress columns with entries < 5 in expected table for chi square test in all digit test!")
    }
  }
  if (TRUE %in% (!(is.na(suppress_first_division_plots)))){
    if (!(is.logical(suppress_first_division_plots))){
      stop("suppress_first_division_plots must be a boolean denoting whether to suppress plots for first and second divisions in all digit test!")
    }
  }
  if (TRUE %in% (!(is.na(suppress_second_division_plots)))){
    if (!(is.logical(suppress_second_division_plots))){
      stop("suppress_second_division_plots must be a boolean denoting whether to suppress plots for second divisions in all digits test!")
    }
  }
}
# tryCatch(2+'2',
#          error = function(e){
#            message("An error occurred:\n", e)
#          },
#          warning = function(w){
#            message("A warning occured:\n", w)
#          },
#          finally = {
#            message("Finally done!")
#          })
