############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# Unpack Round Numbers Test functions in this file
############################################################

#' Get the indexes of round and unround entries in the specified column in \code{digitdata}
#'
#' @inheritParams unpack_round_numbers_test
#'
#' @return Indexes of rounded entries in the specified column
unpacking_round_number_split = function(digitdata, rounding_split_column){
  if (is.na(match(rounding_split_column, colnames(digitdata@cleaned)))) {
    stop('specified category is not a column in the data')
  }
  if (typeof(digitdata@cleaned[[rounding_split_column]]) == "character"){
    stop('the column for splitting unround and round numbers must be a column with numbers')
  }
  rounded_rows = which(digitdata@cleaned[[rounding_split_column]] %% 10 == 0)
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
get_round_unround_digitdata = function(digitdata, rounding_split_column){
  #get the round indexes
  round_numbers_indexes = unpacking_round_number_split(digitdata, rounding_split_column)

  #create a copy of digitdata with only round entries in the specified numeric column
  round_digitdata = make_sub_digitdata(digitdata=digitdata, indexes=round_numbers_indexes)

  #create a copy of digitdata with only unround entries in the specified numeric column
  unround_numbers_indexes = setdiff(1:nrow(digitdata@cleaned), round_numbers_indexes)
  unround_digitdata = make_sub_digitdata(digitdata=digitdata, indexes=unround_numbers_indexes)

  return(list(round_digitdata=round_digitdata, unround_digitdata=unround_digitdata))
}


#' Performs unpack rounded number test by performing all-digit place two-way chi square test vs Benford’s Law.
#' A wrapper function for \code{all_digit_test}.
#'
#' @param rounding_split_column The data column (numeric!) to split rounded and unrounded digits upon to perform unpacking rounding test.
#' @param analysis_columns The names of numeric columns of data to be analyzed. Default can be 'all', where using all data columns in \code{numbers} df in \code{digitdata};
#' an array of column names, as characters; a single column name, as character.
#' @param digit_places The indexes of left-aligned digit places to analyze. There are three options:
#' \itemize{
#'   \item 'all': analzye all digits. Set as default.
#'   \item An numeric array: Perform multiple digit test on the digit places desired to analyze.
#'   \item A number: Perform single digit test on the digit place desired to analyze. If last digit test is desired, pass in -1 or c(-1).
#' }
#' @inheritParams all_digits_test
#'
#' @return
#' \itemize{
#'   \item A list of p-values for round and unround data break by \code{break_out} and \code{category} if specified
#'   \item A list of sample sizes for  round and unround data break by \code{break_out} and \code{category} if specified
#'   \item A list of merged plots, rounded data plots, and un rounded data plots break by \code{break_out} and \code{category} if specified
#'   iff \code{plot = TRUE or 'Save'}
#' }
#' @export
#'
#' @examples
#' unpack_round_numbers_test(digitdata, rounding_split_column='col_name', analysis_columns=c('X', 'Y'))
#' unpack_round_numbers_test(digitdata, rounding_split_column='col_name', digit_places=c(1,2,3), break_out='A', category='Y')
#' unpack_round_numbers_test(digitdata, rounding_split_column='col_name', break_out='A', omit_05=c(0,5), suppress_low_N=TRUE)
unpack_round_numbers_test = function(digitdata, rounding_split_column, analysis_columns='all', digit_places='all', break_out=NA, break_out_grouping=NA,
                                     category=NA, category_grouping=NA, distribution='Benford', contingency_table=NA, plot=TRUE, omit_05=NA,
                                     skip_first_digit=FALSE, skip_last_digit=FALSE, suppress_low_N=FALSE,
                                     suppress_first_division_plots=FALSE, suppress_second_division_plots=TRUE){
  #check input
  input_check(digitdata=digitdata, contingency_table=contingency_table, data_columns=analysis_columns, digit_places=digit_places,
              skip_first_digit=skip_first_digit, omit_05=omit_05, break_out=break_out, break_out_grouping=break_out_grouping,
              distribution=distribution, plot=plot, skip_last_digit=skip_last_digit, rounding_split_column=rounding_split_column,
              suppress_low_N=suppress_low_N, suppress_first_division_plots=suppress_first_division_plots,
              suppress_second_division_plots=suppress_second_division_plots)

  #unpack by round numbers indexes in the specified column
  lst = get_round_unround_digitdata(digitdata, rounding_split_column)
  round_digitdata = lst$round_digitdata
  unround_digitdata = lst$unround_digitdata

  #for plotting to recognize
  round_digitdata@raw = data.frame('round')
  unround_digitdata@raw = data.frame('unround')

  #perform all digit tests for each digitdata object
  round_result = all_digits_test(digitdata=round_digitdata, contingency_table=contingency_table, data_columns=analysis_columns, digit_places=digit_places,
                                   skip_first_digit=skip_first_digit, omit_05=omit_05, break_out=break_out, break_out_grouping=break_out_grouping, category=category,
                                   category_grouping=category_grouping, distribution=distribution, plot=plot, skip_last_digit=skip_last_digit, suppress_low_N=suppress_low_N,
                                   suppress_first_division_plots=suppress_first_division_plots, suppress_second_division_plots=suppress_second_division_plots)
  unround_result = all_digits_test(digitdata=unround_digitdata, contingency_table=contingency_table, data_columns=analysis_columns, digit_places=digit_places,
                                     skip_first_digit=skip_first_digit, omit_05=omit_05, break_out=break_out, break_out_grouping=break_out_grouping, category=category,
                                     category_grouping=category_grouping, distribution=distribution, plot=plot, skip_last_digit=skip_last_digit, suppress_low_N=suppress_low_N,
                                     suppress_first_division_plots=suppress_first_division_plots, suppress_second_division_plots=suppress_second_division_plots)

  merged_plots = list()
  #we should have at least plot for All
  if (plot != FALSE){
    print(paste('Ignore this warning:', "Scale for 'y' is already present. Adding another scale for 'y', which will replace the existing scale."))
    for (break_out_name in names(round_result$plots)){
      for (category_name in names(round_result$plots[[break_out_name]])){
        if (is.na(category)){
          #2 ggplot instance
          round_plot = round_result$plots[[break_out_name]][[category_name]]$aggregate_barplot
          unround_plot = unround_result$plots[[break_out_name]][[category_name]]$aggregate_barplot

          #find max and min y
          ylim_round = ggplot_build(round_plot)$layout$panel_scales_y[[1]]$range$range
          ylim_unround = ggplot_build(unround_plot)$layout$panel_scales_y[[1]]$range$range
          max_y = max(ylim_round[2], ylim_unround[2])
          min_y = min(ylim_round[1], ylim_unround[1])
          merged_plot = plot_multiple_hist2d(list(round=round_plot + scale_y_continuous(breaks=number_ticks(10), expand = expansion(mult = c(0, 0)),
                                                                                        limits = c(min(0, 1.1 * min_y), 1.1 * max_y)),
                                                  unround=unround_plot + scale_y_continuous(breaks=number_ticks(10), expand = expansion(mult = c(0, 0)),
                                                                                             limits = c(min(0, 1.1 * min_y), 1.1 * max_y))))
          merged_plots[[break_out_name]] = merged_plot
          if (plot == TRUE){
            dev.new()
            plot(merged_plot)
          }
        }
      }
    }
  }
  returning_plots = 'No plot with plot=FALSE'
  if (plot != FALSE){
    returning_plots = list(merged=merged_plots, round=round_result$plots, unround=unround_result$plots)
  }
  #merge the results
  return(list(p_values=list(round=round_result$p_values, unround=unround_result$p_values),
              sample_sizes=list(round=round_result$sample_sizes, unround=unround_result$sample_sizes),
              plots=returning_plots))
}
