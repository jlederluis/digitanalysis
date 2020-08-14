############################################################
#Functions for digit analysis R package
###repeat test functions in this file
#Wenjun Chang
#Summer 2020
############################################################

#' Finds the percent of repeats in the given data based on given definition of a repeat (what columns need to match)
#'
#' @param data The The dataframe to compute percent repeat on
#' @inheritParams repeat_test
#'
#' @return The percentage of repeated numbers in input data
find_percent_repeats = function(data, data_columns, round_digit_to_skip){
  #remove all rows with NA entries in numeric columns
  data = data[complete.cases(data[data_columns]), ]
  #drop 0, rounded numbers before analysis
  for (numeric_column in data_columns){
    #drop 0 entiry rows since we dont want to count 0 as repeats
    data = data[data[numeric_column] != 0, ]
    #turn rounded digits to NA if round_digit_to_skip is specified
    if (!(is.na(round_digit_to_skip[1]))){
      #omit all entries with trailing 0 with round_digit_to_skip = 0
      data = data[data[numeric_column] %% 10 != 0, ]
      if (length(round_digit_to_skip) == 2){
        #also omit all entries with trailing 5 with round_digit_to_skip = c(0,5)
        data = data[data[numeric_column] %% 5 != 0, ]
      }
    }
  }
  #find repeats based on specified definition of a repeat
  unique_numbers = dim(unique(data))[1]
  total_numbers = dim(data)[1]
  num_repeats = total_numbers - unique_numbers
  percent_repeats = num_repeats / total_numbers

  #print(data)
  print('unique')
  print(unique_numbers)
  print('total')
  print(total_numbers)

  return(percent_repeats)
}


#' Performs repeat test across \code{break_out} category.
#'
#' @param duplicate_matching_cols An array of names of data columns two rows need to match exactly in order to be defined as a repeat.
#' Default to 'all', meaning matching all columns in 'clean' slot of \code{digitdata}. Must include all columns in \code{data_columns}.
#' @param round_digit_to_skip Whether to omit all rounding numbers in \code{data_columns} represented by numbers ending in 0 or both 0 and 5.
#' \itemize{
#'   \item If omitting rounded numbers as numbers ending in both 0 and 5, pass in c(0,5) or c(5,0)
#'   \item If omitting rounded numbers as numbers ending in only 0 pass in 0 or c(0)
#'   \item If not omitting rounded numbers, pass in NA. Default is NA.
#' }
#' @inheritParams all_digits_test
#'
#' @return
#' \itemize{
#'   \item A table of p-values for repeat test on each category possibly also on sector
#'   \item Plots for each category if \code{plot = TRUE}
#'   \item If NaN is in returned table, it means that there are no occurances of the data in that category --> 0/0 in percentage
#' }
#' @export
#'
#' @examples
#' repeat_test(digitdata)
#' repeat_test(digitdata, duplicate_matching_cols=c('col_name1, col_name2'))
#' repeat_test(digitdata, duplicate_matching_cols=c('col_name1, col_name2'), break_out='col_name')
repeat_test = function(digitdata, data_columns='all', duplicate_matching_cols='all', break_out=NA, break_out_grouping=NA, round_digit_to_skip=c(0,5), plot=TRUE){

  #check input
  input_check(digitdata=digitdata, data_columns=data_columns, break_out=break_out, break_out_grouping=break_out_grouping,
              duplicate_matching_cols=duplicate_matching_cols, plot=plot, omit_05=round_digit_to_skip)

  #handles the data_columns = 'all' situation
  if (data_columns[1] == 'all'){
    data_columns = colnames(digitdata@numbers)
  }

  #handles the duplicate_matching_cols = 'all' situation
  if (duplicate_matching_cols[1] == 'all'){
    duplicate_matching_cols = colnames(digitdata@cleaned)
  }

  #check data_columns is subset of duplicate_matching_cols
  for (column in data_columns){
    if (!(column %in% duplicate_matching_cols)){
      stop('duplicate_matching_cols must include all columns in data_columns!')
    }
  }

  #the columns we want to analyze
  data = digitdata@cleaned[duplicate_matching_cols]

  print('all')

  #percent repeats for all
  percent_repeats_all = find_percent_repeats(data, data_columns, round_digit_to_skip)

  #df to store stats
  percent_repeats_table = data.frame(All=percent_repeats_all)

  #break out by category
  if (!(is.na(break_out))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, break_out, break_out_grouping) #this is a list since unequal number of entries for each category

    #break by category for all
    for (category_name in names(indexes_of_categories)){

      indexes_of_category = indexes_of_categories[[category_name]]
      data_of_category = data.frame(data[indexes_of_category, ])

      percent_repeats_in_category = find_percent_repeats(data_of_category, data_columns, round_digit_to_skip)
      percent_repeats_table[category_name] = percent_repeats_in_category #a value

      print(category_name)
      #print(T %in% is.na(data_of_category[1]))
    }
  }
  #get the mean of all the values computed
  mean_percent_repeated = rowMeans(percent_repeats_table)

  #plot only if we break_out == have > 1 column
  repeat_plot = NA
  if (plot){
    if (!(is.na(break_out))){
      repeat_plot = hist_2D(percent_repeats_table, data_style='row', xlab=break_out, ylab='Percent Repeats', title='Repeats Test',
                            hline=mean_percent_repeated, hline_name='Mean Percentage Repeats')
      print(repeat_plot)
    }
  }
  #create a rowname
  rownames(percent_repeats_table) = 'percent repeated numbers'
  #sort by decreasing rounded percentage
  percent_repeats_table = t(sort(percent_repeats_table, decreasing = TRUE))

  return(list(percent_repeats=percent_repeats_table, plot=repeat_plot))
}

