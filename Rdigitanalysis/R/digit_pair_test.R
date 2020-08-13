############################################################
#Functions for digit analysis R package
###digit pair test functions in this file
#Wenjun Chang
#Summer 2020
############################################################

#' Find the frequency of terminal digit pairs occuring in the data being analzyed
#'
#' @param indexes The indexes of the rows to be analyzed in the data. Used to faciliate \code{break_out} option.
#' Default to NA, meaning using all rows.
#' @inheritParams digit_pairs_test
#'
#' @return An array of [# pairs, # not pairs]
counts_observed = function(digitdata, data_columns, omit_05, min_length, indexes=NA){
  occurances = data.frame(matrix(nrow = 0, ncol = 2))

  for (desired_col in data_columns){
    digit_pair_table = single_column_aligned(digitdata, desired_col, align_diretion='right')

    #for break out, we separate into categories by indexing
    if (!(is.na(indexes[1]))){
      digit_pair_table = digit_pair_table[indexes, ]
    }
    #remove all numbers that has length less than min_length
    digit_pair_table = digit_pair_table[(ncol(digit_pair_table)-min_length+1):ncol(digit_pair_table)]
    ##remove incomplete rows (with nans/length < min_length)
    digit_pair_table = digit_pair_table[complete.cases(digit_pair_table), ]
    #only use last two digits
    digit_pair_table = digit_pair_table[(ncol(digit_pair_table)-1):ncol(digit_pair_table)]

    #need to coerce the names to be identical before rbind
    colnames(digit_pair_table) = colnames(occurances)
    #update
    occurances = rbind(occurances, digit_pair_table)
  }
  #paste the last tywo digits together as numbers
  occurances = as.character(paste(occurances[,1], occurances[,2], sep=''))

  pairs = c('00', '11', '22', '33', '44', '55', '66', '77', '88', '99')
  if (!(is.na(omit_05[1]))){
    #we omit 00 as part of digit pair
    pairs = pairs[-1]
    occurances = occurances[occurances != '00']
    if (length(omit_05) == 2){
      #remove 50 from occurances as well
      occurances = occurances[occurances != '50']
    }
  }
  #find the counts for all valid digit pairs
  counts = table(occurances)
  counts = sum(counts[pairs[pairs %in% names(counts)]])
  return(c(counts, length(occurances) - counts))
}

#get the theoratical frequency of terminal digit pair frequency
freq_true = function(omit_05){
  total = 100 #100 combinations possible
  pairs = 10 #10 pairs possible
  if (!(is.na(omit_05[1]))){
    if (length(omit_05) == 1){
      #omitting 0, so omit 00 pair as option
      total = total - 1
      pairs = pairs - 1
    }
    else {
      #omitting both 0 and 5, so omit 00 pair and 50 as option
      total = total - 2
      pairs = pairs - 1
    }
  }
  return(pairs/total)
}

#' Performs terminal digit pair binomial test vs uniform distribution (Benford’s Law)
#'
#' @param min_length The minimum length of the numbers to be analyzed in digit pair test. Must be an integer >= 2. Default to 3.
#' @inheritParams all_digits_test
#'
#' @return
#' \itemize{
#'   \item A table of p-values for termianl digit pair test on each category
#'   \item Plots for each category if \code{plot = TRUE}
#' }
#' @export
#'
#' @examples
#' digit_pairs_test(digitdata, data_columns='all')
#' digit_pairs_test(digitdata, data_columns=c('col_name1', 'col_name2'))
#' digit_pairs_test(digitdata, data_columns='all', omit_05=NA, min_length=5)
#' digit_pairs_test(digitdata, data_columns='all', omit_05=0, break_out='col_name')
digit_pairs_test = function(digitdata, data_columns='all', omit_05=c(0,5), min_length=3, break_out=NA, break_out_grouping=NA, plot=TRUE){

  #check input
  input_check(digitdata=digitdata, data_columns=data_columns, omit_05=omit_05, break_out=break_out,
              break_out_grouping=break_out_grouping, min_length=min_length, plot=plot)

  #check min_length
  if (is.na(min_length)){
    stop("min_length must be an integer >= 2 denoting the minimum length of the numbers to be analyzed in digit pair test!")
  }
  else if (min_length < 2){
    stop("min_length must be an integer >= 2 denoting the minimum length of the numbers to be analyzed in digit pair test!")
  }
  #get the theoratical frequency based on Benford's Law --> Uniform Distribution
  p = freq_true(omit_05)

  #handle the data_columns = 'all' situation
  data_columns = get_data_columns(digitdata, data_columns)

  #get the observed counts = [success, failrue] for number of terminal digit pairs
  counts = counts_observed(digitdata, data_columns, omit_05, min_length)

  #get p_value from binomial test
  p_value = binom.test(x=counts, p = p, alternative = 'l')$p.value ##############has to specify p = p !!!!!!
  #p_value = pbinom(counts[1], size=sum(counts), prob=p)
  #print(p_value)

  #dataframe of p values to return
  p_values = data.frame(All=p_value)

  #freq of digit pairs for plotting
  freq_digit_pairs = data.frame(All=counts[1]/sum(counts))

  if (!(is.na(break_out))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, break_out, break_out_grouping) #this is a list since unequal number of entries for each category

    #break by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]
      counts_in_category = counts_observed(digitdata, data_columns, omit_05, min_length, indexes_of_category)

      # print(category_name)
      # print(counts_in_category[1])
      # print(sum())

      #p_value_in_category = pbinom(counts_in_category[1], size=sum(counts_in_category), prob=p)
      p_value_in_category = binom.test(x=counts_in_category, p = p, alternative = 'l')$p.value ############
      p_values[category_name] = p_value_in_category #p-value
      freq_digit_pairs[category_name] = counts_in_category[1]/sum(counts_in_category) #for plotting
    }
  }
  #plot only if we break_out == have > 1 column
  digit_pair_plot = NA
  if (plot){
    if (!(is.na(break_out))){
      digit_pair_plot = hist_2D(freq_digit_pairs, data_style='row', xlab=break_out, ylab='Percent Digit Pairs', title='Digit Pairs Test',
                                hline=1/(ncol(freq_digit_pairs)-1), hline_name='Uniform Distribution') #-1 since we want uniform distribution without 'all'
      print(digit_pair_plot)
    }
  }
  return(list(p_values=p_values, plot=digit_pair_plot))
}

