############################################################
#Functions for digit analysis R package
###all digits test main function in this file
#Wenjun Chang
#Summer 2020
############################################################


#' Performs all-digit-place two-way chi square test vs Benford’s Law
#'
#' @param digitdata A object of class \code{DigitAnalysis}.
#' @param contingency_table The Benford probability table.
#' @param data_columns The names of numeric columns of data to be analyzed. Default can be 'all', where using all data columns in \code{numbers} df in \code{digitdata};
#' an array of column names, as characters; a single column name, as character.
#' @param digit_places The indexes of left-aligned digit places to analyze. There are three options:
#' \itemize{
#'   \item 'all': analzye all digits. Set as default.
#'   \item An numeric array: Perform multiple digit test on the digit places desired to analyze.
#'   \item A number: Perform single digit test on the digit place desired to analyze. If last digit test is desired, pass in -1 or c(-1).
#' }
#' @param skip_first_digit TRUE or FALSE: If TRUE, skip last digit place before analysis. Default to FALSE.
#' @param omit_05 Whether to omit 0 or both 0 and 5. If omit both 0 and 5, pass in c(0,5) or c(5,0); if omit only 0 pass in 0 or c(0); if omit neither, pass in NA. Default to c(0,5).
#' @param break_out
#' \itemize{
#'   \item The data column (non-numeric!) to split up the dataset based on different categories in the column if specified as an character.
#'   \item The first division (usually x-axis) shown in plots.
#'   \item Default to NA.
#' }
#' @param distribution 'Benford' or 'Uniform'. Specifies the distribution the chi square test is testing against. Default to 'Benford'.
#' @param plot TRUE or FALSE: If TRUE, skip last digit place before analysis. Default to TRUE.
#' @param skip_last_digit TRUE or FALSE: If TRUE, skip last digit place before analysis, since we don't want tests to overlap. Default to FALSE.
#' \code{skip_last_digit} should overwrite \code{digit_places} and \code{skip_first_digits}.
#' @param standard_df TRUE or FALSE: Default to FALSE.
#'\itemize{
#'   \item TRUE: Computes degrees of freedom for chi square test using formula df = (r-1)(c-1). If table has only one column, df = r - 1.
#'   \item FALSE: Computes degrees of freedom for chi square test using df = r x (c-1). If first digit place is present, df = r x (c-1) - 1.
#' }
#' @param suppress_low_N TRUE or FALSE: If TRUE, suppress columns in expected table
#' if at least one cell in that column has expected value < 5. Default to TRUE.
#'
#' @return
#' \itemize{
#'   \item A table of p-values for all digit tests on each category
#'   \item Plots for each category if \code{plot = TRUE}
#' }
#' @export
#'
#' @examples
#' all_digits_test(digitdata, contingency_table, data_columns='all', digit_places='all', skip_first_digit=TRUE)
#' all_digits_test(digitdata, contingency_table, data_columns='c(col_name1, col_name2)', digit_places=c(1,2,3,5), omit_05=NA, skip_last_digit=TRUE)
#' all_digits_test(digitdata, contingency_table, data_columns='all', digit_places=-1, omit_05=0, break_out='col_name', distribution='Uniform')
all_digits_test = function(digitdata, contingency_table, data_columns='all', digit_places='all', skip_first_digit=FALSE,
                           omit_05=c(0,5), break_out=NA, distribution='Benford', plot=TRUE, skip_last_digit=FALSE, standard_df=FALSE, suppress_low_N=TRUE){

  #check input
  input_check(digitdata=digitdata, contingency_table=contingency_table, data_columns=data_columns, digit_places=digit_places,
              skip_first_digit=skip_first_digit, omit_05=omit_05, break_out=break_out, distribution=distribution, plot=plot,
              skip_last_digit=skip_last_digit, standard_df=standard_df, suppress_low_N=suppress_low_N)

  #######################################################################
  #parse the data
  #######################################################################

  align_direction = 'left'

  #get the digits of the desired data columns to be analyzed
  lst = grab_desired_aligned_columns(digitdata, data_columns, skip_first_digit, skip_last_digit, align_direction)
  digitdata = lst$digitdata
  digits_table = lst$digits_table

  #turn 'all' to an array of numbers if digit_places is 'all'
  if (digit_places[1] == 'all'){
    digit_places = seq(1, digitdata@max)
    if (skip_first_digit){
      digit_places = seq(2, digitdata@max)
    }
  }
  #get usable data
  usable_data = parse_digit_places(digitdata, digits_table, digit_places)

  #parse only needed parts of contingency table
  contingency_table = parse_contigency_table(digitdata, contingency_table, digit_places, skip_first_digit, skip_last_digit, omit_05)

  #get observation table from usable data
  observation_table = obtain_observation(digitdata, usable_data, digit_places, skip_first_digit, skip_last_digit, omit_05)

  #######################################################################
  #do chi square test
  #######################################################################

  #all digit test
  result = chi_square_gof(observation_table, contingency_table, freq=TRUE, suppress_low_N=suppress_low_N, standard=standard_df)
  p_values = data.frame(all=result$p_value)

  #plot
  if (plot){
    plot_all_digit_test(digitdata, result$observed_table, digit_places, title='All')
  }

  #break on category if specified
  if (!(is.na(break_out))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, break_out) #this is a list since unequal number of entries for each category

    #break by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]
      obs_in_category = NA

      #single digit place: need to change the subsetted array into dataframe with colname
      usable_in_category = data.frame(usable_data[indexes_of_category, ])
      colnames(usable_in_category) = colnames(usable_data)
      obs_in_category = obtain_observation(digitdata, usable_in_category, digit_places, skip_first_digit, skip_last_digit, omit_05)

      #chi square test
      result_in_category = chi_square_gof(obs_in_category, contingency_table, freq=TRUE, suppress_low_N=suppress_low_N, standard=standard_df)
      p_values[category_name] = result_in_category$p_value

      # #debug
      # if (category_name == 'Ijara'){
      #   print('Ijara')
      #   print(result_in_category$observed_table)
      #   print(result_in_category$expected_table)
      #   print(sum(result_in_category$observed_table))
      # }

      #plot
      if (plot){
        plot_all_digit_test(digitdata, result_in_category$observed_table, digit_places, title=category_name)
      }
    }
  }
  # print('all digit test')
  # print(p_values)
  return(p_values)
}

