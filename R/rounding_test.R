# ############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# Rounding Test functions in this file
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
    zeros[i] = paste(zeros[i-1], zeros[i], sep='')
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
  #total digits represented by digits that indicate length of number
  total_digits = c()
  #rounded digits represented by digits that indicate length of number
  rounded_digits = c()
  #this should be useless now
  rounding_patterns = stringi::stri_reverse(rounding_patterns[order(nchar(rounding_patterns), rounding_patterns, decreasing=TRUE)])

  for (col_name in colnames(data)){
    #current data column analyzing
    numbers = as.character(data[[col_name]])
    #reverse the data column
    numbers = stringi::stri_reverse(numbers)
    #current total digits
    total_digits_curr = nchar(numbers)
    #current rounded digits
    rounded_digits_curr = rep(0, length(total_digits_curr))

    #check each pattern from longest to shortest to avoid double counting
    for (pattern in rounding_patterns){
      #substring it so that it is of same length with the current ending pattern
      current_data_substrings = substr(numbers, start=1, stop=nchar(pattern))
      #find the numbers with same rounding endstrings
      matched_indexes = which(current_data_substrings == pattern)
      #update numbers; remove them from data so that we don't double count
      if (length(matched_indexes) > 0){
        #update rounded digits: always matches the lonegest pattern --> max
        for (index in matched_indexes){
          #ugly for loop but dont know how to vectorize
          rounded_digits_curr[index] = max(rounded_digits_curr[index], nchar(pattern))
        }
      }
    }
    #ensure all zero entries have rounded == 0
    rounded_digits_curr[(numbers == '0')] = 0
    #update
    total_digits = c(total_digits, total_digits_curr)
    rounded_digits = c(rounded_digits, rounded_digits_curr)
  }
  #might have NA entries
  NA_indexes = which(is.na(total_digits))
  total_digits = total_digits[-NA_indexes]
  rounded_digits = rounded_digits[-NA_indexes]
  return(list(rounded_digits=rounded_digits, total_digits=total_digits, percent_rounded=mean(rounded_digits/total_digits)))
}

#' Performs rounding test vs uniform distribution across categories in a specified data column
#'
#' @param rounding_patterns The patterns to be counted as rounding digits.
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
#'   \item A table of p values of t test for rounding test on each category
#'   \item A table of percent rounded digits for rounding test on each category ordered by decreasing rounded percentage
#'   \item A table of sample sizes for rounding test on each category
#'   \item Plots for each category if \code{plot = TRUE or 'Save'}
#' }
#' @export
#'
#' @examples
#' rounding_test(digitdata)
#' rounding_test(digitdata, omit_05=0)
#' rounding_test(digitdata, omit_05=NA, break_out='col_name')
#' rounding_test(digitdata, data_columns=c('col_name1', 'col_name2'))
rounding_test = function(digitdata, rounding_patterns, break_out, data_columns='all', break_out_grouping=NA, plot=TRUE){
  #check input
  input_check(digitdata=digitdata, contingency_table=NA, data_columns=data_columns, rounding_patterns=rounding_patterns,
              break_out=break_out, break_out_grouping=break_out_grouping, plot=plot)

  #handle the data_columns = 'all' situation
  data_columns = get_data_columns(digitdata, data_columns)

  #the columns we want to analyze
  data = digitdata@numbers[data_columns]

  #rounded digits and total digits for all to do t test
  result_all = compute_percent_rounded_digits(data, rounding_patterns)
  rounded_digits_list = list()
  total_digits_list = list()

  #df to store stats for plotting
  percent_rounded_table = data.frame(All=result_all$percent_rounded)

  #get indexes for each category
  indexes_of_categories = break_by_category(digitdata@cleaned, break_out, break_out_grouping) #this is a list since unequal number of entries for each category

  #break by category for all
  for (category_name in names(indexes_of_categories)){
    indexes_of_category = indexes_of_categories[[category_name]]
    data_of_category = data.frame(data[indexes_of_category, ])
    result_of_category = compute_percent_rounded_digits(data_of_category, rounding_patterns)
    rounded_digits_list[[category_name]] = result_of_category$rounded_digits
    total_digits_list[[category_name]] = result_of_category$total_digits
    percent_rounded_table[category_name] = result_of_category$percent_rounded
  }
  #calculate p value by t test for each category
  p_values = data.frame(matrix(nrow=1, ncol=0))
  rownames(p_values) = 'p_value'

  #sample size of each category
  sample_sizes = data.frame(matrix(nrow=1, ncol=0))
  rownames(sample_sizes) = 'sample size'

  for (category_name in names(rounded_digits_list)){
    category_rounded = rounded_digits_list[[category_name]]/total_digits_list[[category_name]]
    other_rounded = unlist(rounded_digits_list[!(names(rounded_digits_list) %in% c(category_name))],
                           use.names=FALSE) / unlist(total_digits_list[!(names(total_digits_list) %in% c(category_name))],
                                                     use.names=FALSE) #counts in all other categories
    #perform t test
    print(category_rounded)
    print(other_rounded)
    p_value = t.test(category_rounded, other_rounded, alternative = "greater")$p.value
    p_values[category_name] = format_p_values(p_value)
    sample_sizes[category_name] = length(category_rounded)

  }
  if (!(TRUE %in% grepl("\\D", colnames(p_values)))){
    #then it is numeric..sort them
    ordered_cols = c('All', as.character(sort(as.numeric(colnames(p_values)))))
    p_values = p_values[ordered_cols, ]
    percent_repeats_table = percent_repeats_table[c('All', ordered_cols), ]
  }

  #plot only if we break_out == have > 1 column
  rounding_plot = NA
  if (plot != FALSE && !(is.na(break_out))){
    rounding_plot = hist_2D(percent_rounded_table, data_style='row', xlab=break_out, ylab='Percent Rounding',
                            title=paste('Rounding Test \n', 'Broken out by ', break_out, sep=''))
    if (plot == TRUE){
      dev.new()
      print(rounding_plot)
    }
  }
  else {
    #tell user there is no plot
    rounding_plot = 'No plot with plot=FALSE or without break_out'
  }
  #create a rowname
  rownames(percent_rounded_table) = 'percent rounded digits'
  #sort by decreasing rounded percentage
  percent_rounded_table = t(sort(percent_rounded_table, decreasing = TRUE))
  return(list(p_values=p_values, percent_rounded=percent_rounded_table, sample_sizes=sample_sizes, plot=rounding_plot))
}

