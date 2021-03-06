############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# High to Low Digit Test functions in this file
############################################################

#' Computes weighted average probability of high digits across all digit places.
#' Helper function for \code{high_low_by_digit_place}.
#'
#' @param high_and_low_total_counts The total counts for high and low digits in each digit places as a data frame
#' @inheritParams high_low_by_digit_place
#'
#' @return Weighted average probability of high digits across all digit places.
calculate_weighted_p = function(high_and_low_total_counts, high_freq_theoretical){
  #ensure same dimension
  if (length(high_and_low_total_counts) != length(high_freq_theoretical)){
    stop('high_and_low_total_counts and high_freq_theoretical must have same number of columns!')
  }
  #get weighted average probability of high digits
  digit_place_weights = colSums(high_and_low_total_counts) / sum(high_and_low_total_counts)
  weighted_p = sum(digit_place_weights * high_freq_theoretical, na.rm=TRUE)
  return(weighted_p)
}


#' Computes the high low digit binomial test by digit place for desired data columns.
#' Helper function for \code{high_low_test}
#'
#' @param digits_table A digits table for some data columns, preferably returned by \code{grab_desired_aligned_columns}
#' @param high_freq_theoretical A table for theoratical high digit frequency in each digit place.
#' @inheritParams high_low_test
#'
#' @return A table of p_values for input \code{digits_table} by digit place
high_low_by_digit_place = function(digitdata, digits_table, high, high_freq_theoretical, omit_05, skip_first_digit, test_type='binom'){
  #intialize a table for storing total high and low digits counts for each digit place
  high_and_low_total_counts = data.frame(matrix(0, nrow = 2, ncol = digitdata@max))
  #name col and row for debug purpose
  rownames(high_and_low_total_counts) = c('high digits counts', 'low digits counts')
  colnames(high_and_low_total_counts) = digitdata@left_aligned_column_names[1:length(high_and_low_total_counts)]

  #count high low digits in each column
  for (name in colnames(digits_table)){
    for (i in 1:length(digitdata@left_aligned_column_names)){
      if (grepl(digitdata@left_aligned_column_names[i], name, fixed = TRUE)){
        #i is the digit place of this column
        #get frequency of each digit in each digit place
        counts_obs = table(digits_table[name], useNA = 'no')
        if (!(is.na(omit_05[1]))){
          counts_obs = counts_obs[!names(counts_obs) %in% as.character(omit_05)]
        }
        #get total occurances of high digit places
        high_counts_obs = sum(counts_obs[as.character(high)], na.rm = TRUE)
        low_counts_obs = sum(counts_obs, na.rm = TRUE) - high_counts_obs
        #update counts table
        high_and_low_total_counts[i] = high_and_low_total_counts[i] + c(high_counts_obs, low_counts_obs)
      }
    }
  }
  #omit first digit place column if desired
  if (skip_first_digit){
    high_and_low_total_counts = high_and_low_total_counts[-1]
    high_freq_theoretical = high_freq_theoretical[-1]
  }
  p_value = NA
  if (test_type == 'binom'){
    #get weighted values across all digit places
    weighted_p = calculate_weighted_p(high_and_low_total_counts, high_freq_theoretical[1:length(high_and_low_total_counts)])
    total_high_low_count = c(rowSums(high_and_low_total_counts)[1], rowSums(high_and_low_total_counts)[2])
    #binomial test
    p_value = binom.test(total_high_low_count, p = weighted_p, alternative = 'g')$p.value #################
  }
  else if (test_type == 'chisq'){
    expected_freq = rbind(high_freq_theoretical, 1-high_freq_theoretical)[1:length(high_and_low_total_counts)] #high and low digit frequency expected
    rownames(expected_freq) = c('high digits freq', 'low digits freq')
    #chi square test # high_and_low_total_counts is observed table
    p_value = chi_square_gof(high_and_low_total_counts, expected_freq, freq=TRUE, suppress_low_N=FALSE)$p_value
  }
  else {
    stop('test_type can only be either "chisq" or "binom"!')
  }
  sample_sizes = t(as.data.frame(t(colSums(high_and_low_total_counts))))
  observed_high_digits_freq = data.frame(t(high_and_low_total_counts[1, ] / colSums(high_and_low_total_counts)))
  return(list(p_value=format_p_values(p_value), observed_high_digits_freq=observed_high_digits_freq, sample_sizes=sample_sizes, high_freq_theoretical=high_freq_theoretical))
}

#' Perform a single high low test. Helper function for \code{high_low_test}.
#'
#' @inheritParams high_low_test
#'
#' @return p_values table of high low test for input data from \code{digitdata}.
single_high_low_test = function(digitdata, contingency_table, data_columns, high, omit_05, skip_first_digit, skip_last_digit, category, category_grouping, test_type){

  #if omit_05 in high, then should throw error
  for (digit in omit_05){
    if (digit %in% high){
      stop('digits in high should not be omitted.')
    }
  }
  #############################################################
  #get table for the theoratical high to low freqency in each digit place
  #drop X and Digits column of contingency table
  high_freq_theoretical = contingency_table[!(colnames(contingency_table) %in% c('X', 'Digits'))]
  rownames(high_freq_theoretical) = 0:9

  #drop 0 and/or 5
  if (!(is.na(omit_05[1]))){
    high_freq_theoretical = high_freq_theoretical[-(omit_05+1), ]  ### +1 since omit_05 is digits begins with 0, while indexes begins with 1

    #normalize the columns after (if) dropping 0 and/or 5
    for (name in colnames(high_freq_theoretical)){
      #renormialize
      high_freq_theoretical[name] = high_freq_theoretical[name] / sum(high_freq_theoretical[name])
    }
  }
  #get the frequency for high digits in each digit place
  high_freq_theoretical = data.frame(t(colSums(high_freq_theoretical[as.character(high), ])))
  colnames(high_freq_theoretical) = gsub(".", " ", colnames(high_freq_theoretical), fixed=TRUE)
  rownames(high_freq_theoretical) = 'high digits freq'

  #############################################################
  #handle the data_columns = 'all' situation
  data_columns = get_data_columns(digitdata, data_columns)

  #get the data columns desired
  lst = grab_desired_aligned_columns(digitdata, data_columns, skip_first_digit=skip_first_digit, skip_last_digit=skip_last_digit, align_direction='left')
  digits_table = lst$digits_table
  digitdata = lst$digitdata

  #############################################################
  #perform high low test
  result = high_low_by_digit_place(digitdata, digits_table, high, high_freq_theoretical, omit_05, skip_first_digit, test_type)
  p_value = result$p_value
  observed_high_digits_freq = result$observed_high_digits_freq
  sample_sizes = result$sample_sizes

  #create and update tables
  p_values = data.frame(All = p_value)

  high_digits_freq_table = data.frame(matrix(nrow = nrow(observed_high_digits_freq), ncol = 0)) #for plotting
  rownames(high_digits_freq_table) = rownames(observed_high_digits_freq)
  high_digits_freq_table['All'] = observed_high_digits_freq

  sample_sizes_table = data.frame(matrix(nrow = nrow(sample_sizes), ncol = 0)) #for plotting
  rownames(sample_sizes_table) = rownames(sample_sizes)
  sample_sizes_table['All'] = sample_sizes

  #perform a 'year effect' high low test break by category
  if (!(is.na(category))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, category, category_grouping) #this is a list since unequal number of entries for each category
    #break by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]
      data_of_category = data.frame(digits_table[indexes_of_category, ])
      #when do data.frame.... col names changes from A BC to A.BC
      colnames(data_of_category) = gsub("."," ",colnames(data_of_category), fixed=TRUE)

      #get p_values for this category ('year')
      result_of_category = high_low_by_digit_place(digitdata, data_of_category, high, high_freq_theoretical, omit_05, skip_first_digit, test_type)

      #update returning tables
      p_values[category_name] = result_of_category$p_value
      high_digits_freq_table[category_name] = result_of_category$observed_high_digits_freq
      sample_sizes_table[category_name] = result_of_category$sample_sizes
    }
    if (!(TRUE %in% grepl("\\D", colnames(p_values)[-1]))){
      #then it is numeric..sort them
      ordered_columns = c('All', as.character(sort(as.numeric(colnames(p_values)[-1]))))
      p_values = p_values[ordered_columns]
      sample_sizes_table = sample_sizes_table[ordered_columns]
    }
  }
  aggregated_observed_high_freq = data.frame(matrix(nrow=1, ncol=0))
  aggregated_expected_high_freq = data.frame(matrix(nrow=1, ncol=0))
  #compute aggregate expected and observed frequency for high digits
  for (category in colnames(high_digits_freq_table)){
    aggregated_observed_high_freq[category] = sum(high_digits_freq_table[[category]] * sample_sizes_table[[category]] / sum(sample_sizes_table[[category]], na.rm = TRUE), na.rm = TRUE)
    aggregated_expected_high_freq[category] = sum(result$high_freq_theoretical[1:nrow(high_digits_freq_table)] * sample_sizes_table[[category]] / sum(sample_sizes_table[[category]], na.rm = TRUE), na.rm = TRUE)
  }
  if (ncol(aggregated_observed_high_freq) != 1 && !(TRUE %in% grepl("\\D", colnames(aggregated_observed_high_freq)[-1]))){
    #then it is numeric..sort them
    ordered_columns = c('All', as.character(sort(as.numeric(colnames(aggregated_observed_high_freq)[-1]))))
    aggregated_observed_high_freq = aggregated_observed_high_freq[ordered_columns]
    aggregated_expected_high_freq = aggregated_expected_high_freq[ordered_columns]
  }
  return(list(p_values=p_values, high_digits_freq_table=t(high_digits_freq_table), sample_sizes_table=as.data.frame(t(colSums(sample_sizes_table))), high_freq_theoretical=result$high_freq_theoretical,
              aggregated_observed_high_freq=aggregated_observed_high_freq, aggregated_expected_high_freq=aggregated_expected_high_freq))
}


#' Performs high to low digit tests vs probability of high to low digits by Benford's Law via chi square test (default) or binomial test
#'
#' @param high An numeric array of digits or a single number that will be classified as high digits. Defaulted to c(6,7,8,9).
#' @param test_type Specifies whether to perform a binomial test on high vs low digit frequency weighted averaged across digit places with "binom",
#' or a chi square test on high vs. low by each digit place with "chisq". Defaulted to "chisq".
#' @inheritParams all_digits_test
#' @inheritParams sector_test
#'
#' @return
#' \itemize{
#'   \item A table of p-values for high low test on each category
#'   \item A table of sample sizes for high low test on each category
#'   \item Plots for each category if \code{plot = TRUE or 'Save'}
#' }
#' @export
#'
#' @examples
#' high_low_test(digitdata, high=c(5, 6,7,8,9))
#' high_low_test(digitdata, skip_first_digit=TRUE, break_out='col_name', test_type='binom')
#' high_low_test(digitdata, high=c(5,6,9), omit_05=0, skip_last_digit=TRUE, break_out='col_name', category='category_name')
#' high_low_test(digitdata, data_columns='c(col_name1, col_name2)', high=9, break_out='col_name', category='category_name', plot='Save')
high_low_test = function(digitdata, data_columns='all', high=c(6,7,8,9), omit_05=NA, test_type='chisq', distribution='Benford', contingency_table=NA,
                         skip_first_digit=FALSE, skip_last_digit=FALSE, break_out=NA, break_out_grouping=NA, category=NA, category_grouping=NA, plot=TRUE,
                         remove_all_category_visualize=FALSE){
  #check input
  input_check(digitdata=digitdata, contingency_table=contingency_table, data_columns=data_columns, skip_first_digit=skip_first_digit,
              omit_05=omit_05, skip_last_digit=skip_last_digit, high=high, break_out=break_out, break_out_grouping=break_out_grouping,
              category=category, category_grouping=category_grouping, remove_all_category_visualize=remove_all_category_visualize)

  #deal with contingency table and distribution situation
  if (TRUE %in% ((is.na(contingency_table)))){
    #if contingency_table is not passed in, use distribution
    if (tolower(distribution) == 'benford'){
      contingency_table = digitanalysis::benford_table
    }
    else if (tolower(distribution) == 'uniform'){
      contingency_table = digitanalysis::uniform_table
    }
    else {
      stop('contingency_table is invalid, and distribution is not one of "benford" or "uniform"!')
    }
  }
  #perform high low test on all data
  result = single_high_low_test(digitdata, contingency_table, data_columns, high, omit_05, skip_first_digit, skip_last_digit, category, category_grouping, test_type)
  p_values_table = data.frame(matrix(nrow = 0, ncol = ncol(result$p_values))) #p values table
  colnames(p_values_table) = colnames(result$p_values)
  p_values_table['All', ] = result$p_values

  sample_sizes_table = data.frame(matrix(nrow = 0, ncol = ncol(result$sample_sizes_table))) #sample sizes table
  colnames(sample_sizes_table) = colnames(result$sample_sizes_table)
  sample_sizes_table['All', ] = result$sample_sizes_table

  #for plotting
  observed_freq_table = data.frame(All = result$aggregated_observed_high_freq)
  expected_freq_table = data.frame(All = result$aggregated_expected_high_freq)
  rownames(observed_freq_table) = 'All'
  rownames(expected_freq_table) = 'All'
  colnames(observed_freq_table) = colnames(result$aggregated_observed_high_freq)
  colnames(expected_freq_table) = colnames(result$aggregated_expected_high_freq)

  #perform high low test on all break out categories
  if (!(is.na(break_out))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, break_out, break_out_grouping) #this is a list since unequal number of entries for each category

    #break by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]

      #create a digitdata class for this category
      digitdata_of_category = make_sub_digitdata(digitdata=digitdata, indexes=indexes_of_category)

      #perform high low test on this category
      result_of_category = single_high_low_test(digitdata_of_category, contingency_table, data_columns, high, omit_05, skip_first_digit,
                                                skip_last_digit, category, category_grouping, test_type)
      p_values_table[category_name, ][colnames(result_of_category$p_values)] = result_of_category$p_values
      sample_sizes_table[category_name, ][colnames(result_of_category$sample_sizes_table)] = result_of_category$sample_sizes_table

      #for plotting #this is weird af!
      if (ncol(result_of_category$aggregated_observed_high_freq) == 1){
        observed_freq_table[category_name, ] = result_of_category$aggregated_observed_high_freq
        expected_freq_table[category_name, ] = result_of_category$aggregated_expected_high_freq
      }
      else {
        observed_freq_table[category_name, ][colnames(result_of_category$aggregated_observed_high_freq)] = result_of_category$aggregated_observed_high_freq
        expected_freq_table[category_name, ][colnames(result_of_category$aggregated_expected_high_freq)] = result_of_category$aggregated_expected_high_freq
      }
    }

    if (!(TRUE %in% grepl("\\D", rownames(p_values_table)[-1]))){
      #then it is numeric..sort them
      ordered_rows = c('All', as.character(sort(as.numeric(rownames(p_values_table)[-1]))))
      p_values_table = p_values_table[ordered_rows, ]
      sample_sizes_table = sample_sizes_table[ordered_rows, ]
      observed_freq_table = observed_freq_table[ordered_rows, ]
    }
  }

  high_low_plot = 'No plot with plot=FALSE or without break_out'
  if (plot != FALSE){
    plot_title = 'High Low Test \n'
    if (!is.na(break_out)){
      plot_title = paste(plot_title, 'Broken out by ', break_out, sep='')
      if (!is.na(category)){
        plot_title = paste(plot_title, ', ', category, sep='')
      }
    }
    plot_data = observed_freq_table

    if (ncol(expected_freq_table) != 1){
      #remove col for 'All' if we do not want to visualize that
      if (remove_all_category_visualize){
        plot_data = observed_freq_table[!(colnames(observed_freq_table) %in% c('All'))]
      }
      #2D plot with variables
      high_low_plot = hist_2D_variables(plot_data, data_style='col', xlab=break_out, ylab='High Digits Frequency', title=plot_title)
    }
    else {
      #2D plot
      dist_line = geom_line(data = data.frame(x=rownames(expected_freq_table), y=expected_freq_table[[1]]), aes(x = x, y = y, group=1, linetype='Expected High Digits Frequency'), color='red', lwd=1)
      high_low_plot = hist_2D(plot_data, data_style='col', xlab=break_out, ylab='High Digits Frequency', title=plot_title, abline = dist_line)
    }

    if (plot == TRUE){
      dev.new()
      print(high_low_plot)
    }
  }
  return(list(p_values=p_values_table, percent_high_digits=observed_freq_table, sample_sizes=sample_sizes_table, statistical_test=test_type, plot=high_low_plot))
}

