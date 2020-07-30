# ############################################################
# #Functions for digit analysis R package
# ###rounding test functions in this file
# #Wenjun Chang
# #Summer 2020
# ############################################################

#omit_05 means if we count which of trailing 0 or 5 as rounded

#' Compute the percent of rounded digits in a given dataframe.
#' Removes all 0 entries before begin.
#'
#' @param data The dataframe to analyze percent rounded digits on
#' @inheritParams rounding_test
#'
#' @return The percentage of rounded digits in input data
compute_percent_rounded_digits = function(data, omit_05) {
  total_digits = 0
  total_rounded = 0
  for (i in 1:ncol(data)){

    column = as.character(data[[i]])
    #need remove all zeros
    column = column[!(column=='0')]

    no_trailing_zeros = sub("0*$", "", column) #data column without trailing 0s
    num_digits = sum(nchar(column)) #number of digits in this column
    num_rounded_digits =  num_digits - sum(nchar(no_trailing_zeros)) #number of zeros in this column

    if (length(omit_05) == 2){
      #count if last digit in data column without trailing 0s is 5
      num_fives = length(which((as.numeric(no_trailing_zeros) %% 5) == 0))
      num_rounded_digits = num_rounded_digits + num_fives
    }
    total_digits = total_digits + num_digits
    total_rounded = total_rounded + num_rounded_digits
  }
  percent_rounded = total_rounded / total_digits
  return(percent_rounded)
}

################################################
################main function###################
################################################

####
#need ADD distribution and plot parameter
####

#' Performs rounding test vs uniform distribution across categories in a specified data column
#'
#' @inheritParams all_digits_test
#'
#' @return
#' \itemize{
#'   \item A table of p-values for rounding test on each category ordered by decreasing rounded percentage
#'   \item Plots for each category if \code{plot = TRUE}
#' }
#' @export
#'
#' @examples
#' rounding_test(digitdata)
#' rounding_test(digitdata, omit_05=0)
#' rounding_test(digitdata, omit_05=NA, break_out='col_name')
#' rounding_test(digitdata, data_columns=c('col_name1', 'col_name2'))
rounding_test = function(digitdata, data_columns='all', omit_05=c(0,5), break_out=NA){

  #check input
  input_check(digitdata=digitdata, contingency_table=NA, data_columns=data_columns, omit_05=omit_05, break_out=break_out)

  #handle the data_columns = 'all' situation
  data_columns = get_data_columns(digitdata, data_columns)

  #the columns we want to analyze
  data = digitdata@cleaned[data_columns]

  #rounded digits for all
  percent_rounded_all = compute_percent_rounded_digits(data, omit_05)

  #df to store stats
  percent_rounded_table = data.frame(all=percent_rounded_all)

  #break out by category
  if (!(is.na(break_out))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, break_out) #this is a list since unequal number of entries for each category

    #break by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]
      data_of_category = data.frame(data[indexes_of_category, ])
      percent_rounded_in_category = compute_percent_rounded_digits(data_of_category, omit_05)
      percent_rounded_table[category_name] = percent_rounded_in_category
    }
  }

  #get the mean of all the values computed
  mean_percent_rounded = rowMeans(percent_rounded_table)
  percent_rounded_table['mean'] = mean_percent_rounded

  #create a rowname
  rownames(percent_rounded_table) = 'percent rounded digits'
  #sort by decreasing rounded percentage
  percent_rounded_table = t(sort(percent_rounded_table, decreasing = TRUE))
  return(percent_rounded_table)
}

