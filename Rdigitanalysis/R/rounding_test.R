# ############################################################
# #Functions for digit analysis R package
# ###rounding test functions in this file
# #Wenjun Chang
# #Summer 2020
# ############################################################


#' Gives an array of strings of zeros from length 1 to n.
#' Helper function for users inputing \code{rounding_patterns} for \code{rounding_test}.
#'
#' @param n The maximum length of 0s string to generate.
#'
#' @return An array of strings of zeros from length 1 to n.
#' @export
#'
#' @examples
#' n_zeros_pattern(10)
n_zeros_pattern = function(n){
  zeros = rep('0', n)
  for (i in 2:n){
    zeros[i] = paste(zeros[i-1], zeros[i],sep='')
  }
  return(zeros)
}


#' Compute the percent of rounded digits in a given dataframe.
#' Removes all 0 entries before begin.
#'
#' @param data The dataframe to analyze percent rounded digits on
#' @inheritParams rounding_test
#'
#' @return The percentage of rounded digits in input data
compute_percent_rounded_digits = function(data, rounding_patterns) {
  #count for total digits
  total_digits = 0
  #count for rounded digits
  rounded_digits = 0
  #sort the patterns in decreasing order
  rounding_patterns = stringi::stri_reverse(rounding_patterns[order(nchar(rounding_patterns), rounding_patterns, decreasing=TRUE)])

  for (col_name in colnames(data)){
    #current data column analyzing
    numbers = as.character(data[[col_name]])
    #remove all zero entries
    numbers = numbers[!(numbers == '0')]

    #reverse the data column
    numbers = stringi::stri_reverse(numbers)
    #update total digits
    total_digits = sum(nchar(numbers), na.rm=TRUE)

    #check each pattern from longest to shortest to avoid double counting
    for (pattern in rounding_patterns){
      #substring it so that it is of same length with the current ending pattern
      current_data_substrings = substr(numbers, start=1, stop=nchar(pattern))
      #find the numbers with same rounding endstrings
      matched_indexes = which(current_data_substrings == pattern)
      #update numbers; remove them from data so that we don't double count
      if (length(matched_indexes) > 0){
        numbers = numbers[-matched_indexes]
      }
      #update total digits
      rounded_digits = rounded_digits + length(matched_indexes) * nchar(pattern) #number of matched instances x length of the pattern
    }
  }
  return(rounded_digits/total_digits)
}


####
#need ADD distribution and plot parameter
####

#' Performs rounding test vs uniform distribution across categories in a specified data column
#'
#' @inheritParams all_digits_test
#' @param rounding_patterns The patterns to be counted as rounding digits.
#' \itemize{
#'   \item An array of characters such as c('0','00','000','5','50','500', '75', '25').
#'   \item Defaulted to c('0','00','000','0000', '00000', '000000', '5', '50', '500').
#'   \ietm \code{n_zeros_pattern} might be helpful for generating strings of 0s.
#' }
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
rounding_test = function(digitdata, data_columns='all', rounding_patterns=c('0','00','000','0000', '00000', '000000', '5', '50', '500'), break_out=NA){

  #check input
  input_check(digitdata=digitdata, contingency_table=NA, data_columns=data_columns, rounding_patterns=rounding_patterns, break_out=break_out)

  #handle the data_columns = 'all' situation
  data_columns = get_data_columns(digitdata, data_columns)

  #the columns we want to analyze
  data = digitdata@numbers[data_columns]

  #rounded digits for all
  percent_rounded_all = compute_percent_rounded_digits(data, rounding_patterns)

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
      percent_rounded_in_category = compute_percent_rounded_digits(data_of_category, rounding_patterns)
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

  #plot only if we break_out == have > 1 column
  if (!(is.na(break_out))){
    print(hist_2D(percent_rounded_table, data_style='col', xlab=break_out, ylab='percent rounding', title='Rounding Test',
                  hline=mean_percent_rounded, hline_name='Mean Percentage Rounding'))
  }
  return(percent_rounded_table)
}

