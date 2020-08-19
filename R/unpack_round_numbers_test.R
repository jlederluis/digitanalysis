############################################################
#Functions for digit analysis R package
###unpack round numbers test functions in this file
#Wenjun Chang
#Summer 2020
############################################################


############################################################
#unpack round numbers test
############################################################

#' Get the indexes of round and unround entries in the specified column in \code{digitdata}
#'
#' @inheritParams unpack_round_numbers_test
#'
#' @return Indexes of rounded entries in the specified column
unpacking_round_number_split = function(digitdata, unpacking_rounding_column){
  if (is.na(match(unpacking_rounding_column, colnames(digitdata@cleaned)))) {
    stop('specified category is not a column in the data')
  }
  if (typeof(digitdata@cleaned[[unpacking_rounding_column]]) == "character"){
    stop('the column for splitting unround and round numbers must be a column with numbers')
  }
  rounded_rows = which(digitdata@cleaned[[unpacking_rounding_column]] %% 10 == 0)
  return(rounded_rows)
}


#' Creates two two copies of digitdata:
#' \itemize{
#'   \item A copy with only round entries in the specified numeric column
#'   \item A copy with only unround entries in the specified numeric column
#' }
#' Helper function for \code{unpack_round_numbers_test}.
#'
#' @inheritParams unpack_round_numbers_test
#'
#' @return A list with two DigitAnalysis objects: round_digitdata and unround_digitdata
get_round_unround_digitdata = function(digitdata, unpacking_rounding_column){
  #get the round indexes
  round_numbers_indexes = unpacking_round_number_split(digitdata, unpacking_rounding_column)

  #create a copy of digitdata with only round entries in the specified numeric column
  round_digitdata = make_sub_digitdata(digitdata=digitdata, indexes=round_numbers_indexes)

  #create a copy of digitdata with only unround entries in the specified numeric column
  unround_numbers_indexes = setdiff(1:nrow(digitdata@cleaned), round_numbers_indexes)
  unround_digitdata = make_sub_digitdata(digitdata=digitdata, indexes=unround_numbers_indexes)

  return(list(round_digitdata=round_digitdata, unround_digitdata=unround_digitdata))
}


#' Performs unpacking unround number test by performing all-digit place two-way chi square test vs Benford’s Law.
#' A wrapper function for \code{all_digit_test}.
#'
#' @param unpacking_rounding_column The data column (numeric!) to split rounded and unrounded digits upon to perform unpacking rounding test.
#' @inheritParams all_digits_test
#'
#' @return A dataframe of p values on rounded and unrounded digits (also by category if break_out is specified)
#' @export
#'
#' @examples
#' unpack_round_numbers_test(digitdata, contingency_table, unpacking_rounding_column='Column Name', data_columns='all', digit_places='all')
#' unpack_round_numbers_test(digitdata, contingency_table, unpacking_rounding_column='Column Name', data_columns='all', digit_places=-1)
#' unpack_round_numbers_test(digitdata, contingency_table, unpacking_rounding_column='Column Name', data_columns='all', digit_places=c(1,2,3), omit_05=NA)
unpack_round_numbers_test = function(digitdata, contingency_table=NA, unpacking_rounding_column, data_columns='all', digit_places='all',
                                     skip_first_digit=FALSE, omit_05=c(0,5), break_out=NA, break_out_grouping=NA, category=NA, category_grouping=NA,
                                     distribution='Benford', plot=TRUE, skip_last_digit=FALSE, standard_df=FALSE, suppress_low_N=TRUE,
                                     suppress_first_division_plots=FALSE, suppress_second_division_plots=TRUE){
  #check input
  input_check(digitdata=digitdata, contingency_table=contingency_table, data_columns=data_columns, digit_places=digit_places,
              skip_first_digit=skip_first_digit, omit_05=omit_05, break_out=break_out, break_out_grouping=break_out_grouping,
              distribution=distribution, plot=plot, skip_last_digit=skip_last_digit, unpacking_rounding_column=unpacking_rounding_column,
              standard_df=standard_df, suppress_low_N=suppress_low_N)

  #unpack by round numbers indexes in the specified column
  lst = get_round_unround_digitdata(digitdata, unpacking_rounding_column)
  round_digitdata = lst$round_digitdata
  unround_digitdata = lst$unround_digitdata

  #for plotting to recognize
  round_digitdata@raw = data.frame('round')
  unround_digitdata@raw = data.frame('unround')

  #perform all digit tests for each digitdata object
  round_p_values = all_digits_test(round_digitdata, contingency_table, data_columns, digit_places, skip_first_digit,
                                   omit_05, break_out, break_out_grouping, category, category_grouping, distribution,
                                   plot, skip_last_digit, standard_df, suppress_low_N, suppress_first_division_plots,
                                   suppress_second_division_plots)
  unround_p_values = all_digits_test(unround_digitdata, contingency_table, data_columns, digit_places, skip_first_digit,
                                     omit_05, break_out, break_out_grouping, category, category_grouping, distribution,
                                     plot, skip_last_digit, standard_df, suppress_low_N, suppress_first_division_plots,
                                     suppress_second_division_plots)

  #merge the results
  #p_values = rbind(round_p_values, unround_p_values)
  #rownames(p_values) = c('round', 'unround')
  #print('unpack round numbers test')
  #print(p_values)
  return(list(round=round_p_values, unround=unround_p_values))
}
