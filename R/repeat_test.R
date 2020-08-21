############################################################
#Functions for digit analysis R package
###repeat test functions in this file
#Wenjun Chang
#Summer 2020
############################################################

#' Omits the numbers that are qualified as rounded numbers specified by \code{rounding_patterns_to_omit}.
#'
#' @inheritParams find_percent_repeats
#'
#' @return \code{data} without round numbers.
omit_rounded_numbers = function(data, data_columns, rounding_patterns_to_omit){
  #sort the patterns in decreasing order
  rounding_patterns_to_omit = stringi::stri_reverse(rounding_patterns_to_omit[order(nchar(rounding_patterns_to_omit), rounding_patterns_to_omit, decreasing=TRUE)])
  #data column to omit round numbers
  numbers = as.character(data[[data_columns]])
  #remove all zero entries
  numbers = numbers[!(numbers == '0')]
  #reverse the data column
  numbers = stringi::stri_reverse(numbers)

  #check each pattern from longest to shortest to avoid double counting
  for (pattern in rounding_patterns_to_omit){
    #substring it so that it is of same length with the current ending pattern
    current_data_substrings = substr(numbers, start=1, stop=nchar(pattern))
    #find the numbers with same rounding endstrings
    matched_indexes = which(current_data_substrings == pattern)
    #update numbers; remove rounded rows from data
    if (length(matched_indexes) > 0){
      numbers = numbers[-matched_indexes]

      temp_data = as.data.frame(data[-matched_indexes, ]) #OMG why do I have to do this!!!
      colnames(temp_data) = colnames(data)
      data = temp_data

      # print(names(data))
    }
  }
  return(data)
}

#' Finds the percent of repeats in the given data based on given definition of a repeat (what columns need to match)
#'
#' @param data The The dataframe to compute percent repeat on
#' @inheritParams repeat_test
#'
#' @return The percentage of repeated numbers in input data
find_percent_repeats = function(data, data_columns, rounding_patterns_to_omit){
  #omit rounded numbers
  if (!any(is.na(rounding_patterns_to_omit)) && !is.na(data_columns)){
    data = omit_rounded_numbers(data, data_columns, rounding_patterns_to_omit)
  }
  #find repeats based on specified definition of a repeat
  unique_numbers = dim(unique(data))[1]
  total_numbers = dim(data)[1]
  num_repeats = total_numbers - unique_numbers
  percent_repeats = num_repeats / total_numbers

  #1 = repeat, 0 = not repeat
  repeats_count = c(rep(0, unique_numbers), rep(1, num_repeats))

  # print('unique')
  # print(unique_numbers)
  # print('total')
  # print(total_numbers)

  return(list(repeats_count=repeats_count, percent_repeats=percent_repeats))
}


#' Performs repeat test across \code{break_out} category.
#'
#' @param duplicate_matching_cols An array of names of data columns two rows need to match exactly in order to be defined as a repeat.
#' Must include all columns in \code{data_columns}. Default to 'all', meaning matching all columns in 'number' slot of \code{digitdata}.
#' @param rounding_patterns_to_omit The patterns to be counted as rounding digits to skip. Defaulted to NA.
#' \itemize{
#'   \item An array of characters such as c('0','00','000','5','50','500', '75', '25').
#'   \item \code{n_zeros_pattern} might be helpful for generating strings of 0s.
#' }
#' @param break_out
#' \itemize{
#'   \item The data column (non-numeric!) to split up the dataset based on different categories in the column if specified as an character.
#'   \item The first division (usually x-axis) shown in plots.
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
repeat_test = function(digitdata, break_out, data_columns=NA, duplicate_matching_cols='all', break_out_grouping=NA, rounding_patterns_to_omit=NA, plot=TRUE){
  #data_columns should be the column to look at rounded digits on
  #check input
  input_check(digitdata=digitdata, data_columns=data_columns, break_out=break_out, break_out_grouping=break_out_grouping,
              duplicate_matching_cols=duplicate_matching_cols, plot=plot, rounding_patterns=rounding_patterns_to_omit)

  # #handles the data_columns = 'all' situation
  # data_columns = get_data_columns(digitdata, data_columns)

  if (is.na(data_columns) && !(is.na(rounding_patterns_to_omit))){
    warning("Are you sure to not specify data_columns? rounding_patterns_to_omit omit rounded numbers in data_columns!")
  }

  if (is.na(rounding_patterns_to_omit) && !(is.na(data_columns))){
    warning("Are you sure to not specify rounding_patterns_to_omit? Rounded numbers in data_columns are omitted based on rounding_patterns_to_omit!")
  }


  #handles the duplicate_matching_cols = 'all' situation
  if (duplicate_matching_cols[1] == 'digit_columns'){
    duplicate_matching_cols = get_data_columns(digitdata, duplicate_matching_cols)
  }
  #check data_columns is subset of duplicate_matching_cols
  if (!is.na(data_columns)){
    for (column in data_columns){
      if (!(column %in% duplicate_matching_cols)){
        stop('The argument duplicate_matching_cols must include all column(s) where you are analyzing digit data!')
      }
    }
  }

  #the columns we want to analyze
  data = digitdata@cleaned[duplicate_matching_cols]

  #repeats counts for all
  result_all = find_percent_repeats(data, data_columns, rounding_patterns_to_omit)

  #intialize a dataframe for counts of repeat vs. not repeat
  repeats_count_all = list()

  #df to store stats for plotting
  percent_repeats_table = data.frame(All=result_all$percent_repeats)

  #return(repeats_count_all)

  #get indexes for each category
  indexes_of_categories = break_by_category(digitdata@cleaned, break_out, break_out_grouping) #this is a list since unequal number of entries for each category

  #break by category and find counts of repeat vs. not repeat
  for (category_name in names(indexes_of_categories)){
    indexes_of_category = indexes_of_categories[[category_name]]
    data_of_category = as.data.frame(data[indexes_of_category, ])
    colnames(data_of_category) = colnames(data) #since find_percent_repeats depends on column name
    result_of_category = find_percent_repeats(data_of_category, data_columns, rounding_patterns_to_omit)
    repeats_count_all[[category_name]] = result_of_category$repeats_count
    percent_repeats_table[category_name] = result_of_category$percent_repeats
  }

  #calculate p value by t test for each category
  p_values = data.frame(matrix(nrow=1, ncol=0))
  rownames(p_values) = 'p_value'
  #return(repeats_count_all)

  for (category_name in names(repeats_count_all)){
    category_counts = repeats_count_all[[category_name]]
    other_counts = unlist(repeats_count_all[!(names(repeats_count_all) %in% c(category_name))], use.names=FALSE) #counts in all other categories
    #perform t test
    # print(category_name)
    # print(sum(category_counts==0))
    # print(sum(category_counts==1))
    # print(sum(other_counts==0))
    # print(sum(other_counts==1))
    if (length(category_counts) < 2){
      p_value = NA #cannot perform t test...x too small
    }
    else {
      p_value = t.test(category_counts, other_counts, alternative = "greater")$p.value
    }
    p_values[category_name] = p_value
  }
#   #break out by category
#   if (!(is.na(break_out))){
#     #get indexes for each category
#     indexes_of_categories = break_by_category(digitdata@cleaned, break_out, break_out_grouping) #this is a list since unequal number of entries for each category
#
#     #break by category for all
#     for (category_name in names(indexes_of_categories)){
#
#       indexes_of_category = indexes_of_categories[[category_name]]
#       data_of_category = as.data.frame(data[indexes_of_category, ])
#
#       percent_repeats_in_category = find_percent_repeats(data_of_category, data_columns, rounding_patterns_to_omit)
#       percent_repeats_table[category_name] = percent_repeats_in_category #a value
#
#       print(category_name)
#       #print(T %in% is.na(data_of_category[1]))
#     }
#   }
  #get the mean of all the values computed
  mean_percent_repeated = rowMeans(percent_repeats_table)

  #plot only if we break_out == have > 1 column
  repeats_plot = NA
  if (plot && !(is.na(break_out))){
    repeats_plot = hist_2D(percent_repeats_table, data_style='row', xlab=break_out, ylab='Percent Repeats',
                          title=paste('Repeats Test \n', 'break_out = ', break_out, sep=''),
                          hline=mean_percent_repeated, hline_name='Mean Percentage Repeats')
    dev.new()
    print(repeats_plot)
  }
  else {
    #tell user there is no plot
    repeats_plot = 'No plot with plot=FALSE or without break_out'
  }
  #create a rowname
  rownames(percent_repeats_table) = 'percent repeated numbers'
  #sort by decreasing rounded percentage
  percent_repeats_table = t(sort(percent_repeats_table, decreasing = TRUE))
  return(list(p_values=p_values, percent_repeats=percent_repeats_table, plot=repeats_plot))
}

