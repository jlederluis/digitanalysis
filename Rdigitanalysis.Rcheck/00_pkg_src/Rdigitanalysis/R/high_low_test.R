############################################################
#Functions for digit analysis R package
###high to low digit test functions in this file
#Wenjun Chang
#Summer 2020
############################################################

#' Computes the high low digit binomial test by digit place for desired data columns.
#' Helper function for \code{high_low_test}
#'
#' @param data A digits table for some data columns, preferably returned by \code{grab_desired_aligned_columns}
#' @param high_freq_theoratical A table for theoratical high digit frequency in each digit place.
#' @inheritParams high_low_test
#'
#' @return A table of p_values for input \code{data} by digit place
high_low_by_digit_place = function(digitdata, digits_table, high, high_freq_theoratical, skip_first_digit, omit_05){
  #intialize a table for storing total high and low digits counts for each digit place
  high_and_low_total_counts = data.frame(matrix(0, nrow = 2, ncol = digitdata@max))
  #name col and row for debug purpose
  rownames(high_and_low_total_counts) = c('high digit counts', 'low digits counts')
  colnames(high_and_low_total_counts) = digitdata@left_aligned_column_names[1:length(high_and_low_total_counts)]

  #count high low digits in each column
  for (name in colnames(digits_table)){
    for (i in 1:length(digitdata@left_aligned_column_names)){
      if (grepl(digitdata@left_aligned_column_names[i], name, fixed = TRUE)){
        #i is the digit place of this column

        #get frequency of each digit in each digit place
        counts_obs = table(digits_table[name])
        if (!(is.na(omit_05[1]))){
          counts_obs = counts_obs[-omit_05]
        }
        #get total occurances of high digit places
        high_counts_obs = sum(counts_obs[as.character(high)], na.rm = TRUE)
        low_counts_obs = sum(counts_obs) - high_counts_obs

        #update counts table
        high_and_low_total_counts[i] = high_and_low_total_counts[i] + c(high_counts_obs, low_counts_obs)
      }
    }
  }
  #intilaize p value table
  p_values = data.frame(matrix(nrow = 1, ncol = 0))

  #get p_value from binomial test
  for (i in 1:length(high_and_low_total_counts)){
    p_value = binom.test(high_and_low_total_counts[[i]], p = high_freq_theoratical[i])$p.value
    p_values[[colnames(high_and_low_total_counts)[i]]] = p_value
  }
  #drop 1st digit place if this is true
  if (skip_first_digit){
    p_values = p_values[-1]
  }
  rownames(p_values) = 'p value'
  return(p_values)
}

####
#need ADD plot parameter
####

#' Performs high to low digit tests vs probability of high to low digits by Benford's Law via binomial test
#'
#' @param high An numeric array of digits or a single number that will be classified as high digits. Defaulted to c(6,7,8,9)
#' @inheritParams all_digits_test
#'
#' @return
#' \itemize{
#'   \item A table of p-values for high low tests on each category
#'   \item Plots for each category if \code{plot = TRUE}
#' }
#' @export
#'
#' @examples
#' high_low_test(digitdata, contingency_table, data_columns='all', high=c(6,7,8,9))
#' high_low_test(digitdata, contingency_table, data_columns='all', high=c(8,9), skip_first_digit=TRUE)
#' high_low_test(digitdata, contingency_table, data_columns='all', high=c(5,6,9), omit_05=0, skip_last_digit=TRUE, break_out=NA)
#' high_low_test(digitdata, contingency_table, data_columns='all', high=9, omit_05=NA, skip_last_digit=TRUE, break_out='col_name')
high_low_test = function(digitdata, contingency_table, data_columns='all', high=c(6,7,8,9), omit_05=c(0,5), skip_first_digit=FALSE, skip_last_digit=FALSE, break_out=NA){

  #check input
  input_check(digitdata=digitdata, contingency_table=contingency_table, data_columns=data_columns, skip_first_digit=skip_first_digit,
              omit_05=omit_05, break_out=break_out, skip_last_digit=skip_last_digit, high=high)

  #if omit_05 in high, then should throw error
  for (digit in omit_05){
    if (digit %in% high){
      stop('digits in high should not be omitted.')
    }
  }

  #############################################################
  #get table for the theoratical high to low freqency in each digit place
  #drop X and Digits column of contingency table
  high_freq_theoratical = contingency_table[!(colnames(contingency_table) %in% c('X', 'Digits'))]
  rownames(high_freq_theoratical) = 0:9

  #drop 0 and/or 5
  if (!(is.na(omit_05[1]))){
    high_freq_theoratical = high_freq_theoratical[-(omit_05+1), ]  ### +1 since omit_05 is digits begins with 0, while indexes begins with 1

    #normalize the columns after (if) dropping 0 and/or 5
    for (name in colnames(high_freq_theoratical)){
      #renormialize
      high_freq_theoratical[name] = high_freq_theoratical[name] / sum(high_freq_theoratical[name])
    }
  }

  #get the frequency for high digits in each digit place
  high_freq_theoratical = t(data.frame(colSums(high_freq_theoratical[as.character(high), ])))
  rownames(high_freq_theoratical) = 'high digits freq'

  #############################################################
  #handle the data_columns = 'all' situation
  data_columns = get_data_columns(digitdata, data_columns)

  #get the data columns desired
  lst = grab_desired_aligned_columns(digitdata, data_columns, skip_first_digit=skip_first_digit, skip_last_digit=skip_last_digit, align_direction='left')
  digits_table = lst$digits_table
  digitdata = lst$digitdata

  #############################################################
  #perform high low test
  p_values = data.frame(t(high_low_by_digit_place(digitdata, digits_table, high, high_freq_theoratical, skip_first_digit, omit_05)))
  colnames(p_values) = 'all'

  #perform a 'year effect' high low test break by category
  if (!(is.na(break_out))){
    #get indexes for each category
    indexes_of_categories = break_by_category(data=digitdata@cleaned, break_out=break_out) #this is a list since unequal number of entries for each category

    #break by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]
      data_of_category = data.frame(digits_table[indexes_of_category, ])
      #when do data.frame.... col names changes from A BC to A.BC
      colnames(data_of_category) = gsub("."," ",colnames(data_of_category), fixed=TRUE)

      #get p_values for this category ('year')
      p_values_of_category = high_low_by_digit_place(digitdata, data_of_category, high, high_freq_theoratical, skip_first_digit, omit_05)

      #update returning list
      p_values[category_name] = NA
      p_values[category_name][colnames(p_values_of_category), ] = t(p_values_of_category)
    }
  }
  return(p_values)
}

