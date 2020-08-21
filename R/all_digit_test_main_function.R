############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# All Digits Test main function in this file
############################################################

#' Helper function for \code{all_digits_test} to perform a single all digits test.
#' @param subset_name The main title to put on plots for current test.
#' @inheritParams all_digits_test
#'
#' @return p_values for input data possibly break by \code{category}.
single_all_digits_test = function(digitdata, contingency_table, data_columns, digit_places, skip_first_digit, omit_05, category,
                                  category_grouping, skip_last_digit, suppress_low_N, subset_name, plot,
                                  suppress_second_division_plots){

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
  result = chi_square_gof(observation_table, contingency_table, freq=TRUE, suppress_low_N=suppress_low_N)
  #return(list(a=contingency_table, b=observation_table))
  p_values = data.frame(All=result$p_value)
  all_digit_test_plots = list()

  #plot
  if (plot){
    all_digits_plot = plot_all_digit_test(digitdata, result$observed_table, result$expected_table, digit_places,
                                          title=paste('Broken out by ', subset_name, ', ', 'AllCategory', sep=''))
    all_digit_test_plots[['AllCategory']] = all_digits_plot
  }

  #break on category if specified #do not plot for these
  if (!(is.na(category))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, category, category_grouping) #this is a list since unequal number of entries for each category

    #break by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]
      obs_in_category = NA

      #single digit place: need to change the subsetted array into dataframe with colname
      usable_in_category = as.data.frame(usable_data[indexes_of_category, ])
      colnames(usable_in_category) = colnames(usable_data)
      obs_in_category = obtain_observation(digitdata, usable_in_category, digit_places, skip_first_digit, skip_last_digit, omit_05)

      #chi square test
      result_in_category = chi_square_gof(obs_in_category, contingency_table, freq=TRUE, suppress_low_N=suppress_low_N)
      p_values[category_name] = result_in_category$p_value

      #plot
      if (plot){
        if (!suppress_second_division_plots){
          plot_in_category = plot_all_digit_test(digitdata, result_in_category$observed_table, result_in_category$expected_table, digit_places,
                                                 title=paste('Broken out by ', subset_name, ', ', category_name, sep=''))
          all_digit_test_plots[[category_name]] = plot_in_category
        }
      }
    }
  }
  if (length(all_digit_test_plots) == 0){
    all_digit_test_plots = 'No plots since plot=FALSE'
  }
  return(list(p_values=p_values, plots=all_digit_test_plots))
}



# plotting
#' Performs all-digit-place two-way chi square test vs Benford’s Law
#'
#' @param digitdata A object of class \code{DigitAnalysis}.
#' @param contingency_table The user-input probability table of arbitrary distribution. Overwrites \code{distribution} if not NA.
#' Must be a dataframe of the form as \code{benford_table}. Defaulted to NA.
#' \itemize{
#'   \item Check out \code{load(file = "data/benford_table.RData")} to see the format of \code{benford_table}
#' }
#' @param data_columns The names of numeric columns of data to be analyzed. Default can be 'all', where using all data columns in \code{numbers} df in \code{digitdata};
#' an array of column names, as characters; a single column name, as character.
#' @param digit_places The indexes of left-aligned digit places to analyze. There are three options:
#' \itemize{
#'   \item 'all': analzye all digits. Set as default.
#'   \item An numeric array: Perform multiple digit test on the digit places desired to analyze.
#'   \item A number: Perform single digit test on the digit place desired to analyze. If last digit test is desired, pass in -1 or c(-1).
#' }
#' @param skip_first_digit TRUE or FALSE: If TRUE, skip first digit place before analysis. Default to FALSE.
#' @param omit_05 Whether to omit 0 or both 0 and 5. If omit both 0 and 5, pass in c(0,5) or c(5,0); if omit only 0 pass in 0 or c(0); if omit neither, pass in NA. Default to NA.
#' @param break_out
#' \itemize{
#'   \item The data column (non-numeric!) to split up the dataset based on different categories in the column if specified as an character.
#'   \item The first division (usually x-axis) shown in plots.
#'   \item Default to NA.
#' }
#' @param break_out_grouping A list of arrays, or defaulted to NA. Only effective if \code{break_out} is not NA.
#' \itemize{
#'   \item Each the names of the elements in the list is the break_out name
#'   \item Each array contains the values belonging to that break_out
#'   \item If it is remain as NA as default, while \code{break_out} is not NA, then \code{break_out_grouping} will default to every individual item in
#'   \code{break_out} will be in a separate group.
#' }
#' @param category The column for splitting the data into sectors for separate analysis. The second division (usually variables) shown in plots.
#' @param category_grouping A list of arrays, or defaulted to NA. Only effective if \code{category} is not NA.
#' \itemize{
#'   \item Each the names of the elements in the list is the category name
#'   \item Each array contains the values belonging to that category
#'   \item If it is remain as NA as default, while \code{category} is not NA, then \code{category_grouping} will default to every individual item in
#'   \code{category} will be in a separate group.
#'   \item e.g. \code{category_grouping = list(group_1=c(category_1, category_2, ...), group_2=c(category_10, ...), group_3=c(...))}
#' }
#' @param distribution 'Benford' or 'Uniform'. Case insensitive. Specifies the distribution the chi square test is testing against. Default to 'Benford'.
#' @param plot TRUE or FALSE: If TRUE, skip last digit place before analysis. Default to TRUE.
#' @param skip_last_digit TRUE or FALSE: If TRUE, skip last digit place before analysis, since we don't want tests to overlap. Default to FALSE.
#' \code{skip_last_digit} should overwrite \code{digit_places} and \code{skip_first_digits}.
#' @param suppress_low_N TRUE or FALSE: If TRUE, suppress columns in expected table
#' if at least one cell in that column has expected value < 5. Default to FALSE.
#' @param suppress_first_division_plots TRUE or FALSE: If TRUE, suppress all plots on first and second division.
#' If TRUE, \code{suppress_second_division_plots} will also be set to TRUE.
#' @param suppress_second_division_plots TRUE or FALSE: If TRUE, suppress all plots on second division.
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
all_digits_test = function(digitdata, data_columns='all', digit_places='all', break_out=NA, break_out_grouping=NA, category=NA, category_grouping=NA,
                           distribution='Benford', contingency_table=NA, plot=TRUE, omit_05=NA, skip_first_digit=FALSE, skip_last_digit=FALSE,
                           suppress_low_N=FALSE, suppress_first_division_plots=FALSE, suppress_second_division_plots=TRUE){
  #check input
  input_check(digitdata=digitdata, contingency_table=contingency_table, data_columns=data_columns, digit_places=digit_places,
              skip_first_digit=skip_first_digit, omit_05=omit_05, break_out=break_out, break_out_grouping=break_out_grouping,
              distribution=distribution, plot=plot, skip_last_digit=skip_last_digit, suppress_low_N=suppress_low_N,
              category=category, category_grouping=category_grouping, suppress_first_division_plots=suppress_first_division_plots,
              suppress_second_division_plots=suppress_second_division_plots)

  #handles the data_columns = 'all' situation
  data_columns = get_data_columns(digitdata, data_columns)

  #check if data_columns are valid
  for (desired_col in data_columns){
    if (is.na(match(desired_col, colnames(digitdata@numbers)))){
      #throw error
      error_message = paste(deparse(substitute(desired_col)), " was not specified as a data column in the DigitAnalysis object ",
                            deparse(substitute(digitdata)), ". You can re-run process_digit_data, or specify a different column to analyze.", sep="")
      stop(error_message)
    }
  }

  #deal with contingency table and distribution situation
  if (TRUE %in% ((is.na(contingency_table)))){
    #if contingency_table is not passed in, use distribution
    if (tolower(distribution) == 'benford'){
      data("benford_table")
      contingency_table = benford_table
    }
    else if (tolower(distribution) == 'uniform'){
      data("uniform_table")
      contingency_table = uniform_table
    }
    else {
      stop('contingency_table is invalid, and distribution is not one of "benford" or "uniform"!')
    }
  }
  #trivial plotting arg
  if (suppress_first_division_plots){
    suppress_second_division_plots = TRUE
  }
  #perform digit test for all
  result_all = single_all_digits_test(digitdata, contingency_table, data_columns, digit_places, skip_first_digit, omit_05,
                                      category, category_grouping, skip_last_digit, suppress_low_N, subset_name='AllBreakout',
                                      plot=plot, suppress_second_division_plots=suppress_second_division_plots)
  #return(result_all)
  p_values_all = result_all$p_values
  plots_all = result_all$plots

  #p values to return
  p_values = data.frame(matrix(nrow = 0, ncol = ncol(p_values_all)))
  colnames(p_values) = colnames(p_values_all)
  p_values['All', ] = p_values_all

  #plots to return
  plots = list(AllBreakout = plots_all)

  if (suppress_first_division_plots){
    plot = FALSE #do not plot for the for loop
  }
  #break on break out category if specified
  if (!(is.na(break_out))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, break_out, break_out_grouping) #this is a list since unequal number of entries for each category

    #break by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]
      digitdata_of_category = make_sub_digitdata(digitdata, indexes_of_category)

      result_of_category = single_all_digits_test(digitdata_of_category, contingency_table, data_columns, digit_places, skip_first_digit, omit_05,
                                                  category, category_grouping, skip_last_digit, suppress_low_N, subset_name=category_name,
                                                  plot=plot, suppress_second_division_plots=suppress_second_division_plots)
      p_values_of_category = result_of_category$p_values
      plots_of_category = result_of_category$plots

      p_values[category_name, ] = p_values_of_category
      plots[[category_name]] = plots_of_category
    }
  }
  return(list(p_values=p_values, plots=plots))
}

