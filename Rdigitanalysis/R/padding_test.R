############################################################
#Functions for digit analysis R package
###padding test functions in this file
#Wenjun Chang
#Summer 2020
############################################################



############################################################
#helper functions
############################################################


#' Obtains Benford mean in each digit place after specifying omitting 0 and 5 or not
#'
#' @inheritParams padding_test
#'
#' @return A table for Benford mean in each digit place after removing 0 and/or 5 if desired
#' @export
get_benford_mean = function(contingency_table, omit_05=c(0,5)){
  # #check input
  # input_check(contingency_table=contingency_table, omit_05=omit_05)

  #create a table for the mean of benford distribution in each digit place
  names = colnames(contingency_table)[!(colnames(contingency_table) %in% c("X", "Digits"))]
  Benford_mean = data.frame(matrix(nrow = 1, ncol = length(names)))
  colnames(Benford_mean) = names
  rownames(Benford_mean) = 'mean'

  #modify contingency table with omit_05
  ### +1 since omit_05 is digits begins with 0, while indexes begins with 1
  if (!(is.na(omit_05[1]))){
    contingency_table = contingency_table[-(omit_05+1), ]
  }
  #/ sum(contingency_table[name] to normailize if p does not sum to 1
  for (name in colnames(Benford_mean)){
    #renormalize
    contingency_table[name] = contingency_table[name] / sum(contingency_table[name])
    #get mean
    Benford_mean[name] = sum(contingency_table[name] * contingency_table['Digits'])
  }
  return(list(Benford_mean=Benford_mean, contingency_table=contingency_table))
}


#' Fetches the merged rows of all data columns aligned-right digits. MIGHT BE USEFUL IN ADT!
#'
#' @param indexes Defaulted to NA. If NA, uses all rows. If specified, only uses the specified rows.
#' @inheritParams padding_test
#'
#' @return Combined data for \code{data_columns} by merging rows
#' @export
combine_by_columns = function(digitdata, data_columns, indexes=NA){
  data = single_column_aligned(digitdata, desired_col=data_columns[1], align_diretion='right')
  #get subset of data if specified
  if (!(is.na(indexes[1]))){
    data = data[indexes, ]
  }
  colnames(data) = rev(digitdata@right_aligned_column_names[1:length(data)])

  if (length(data_columns) > 1){
    for (i in 2:length(data_columns)){
      data2 = single_column_aligned(digitdata, desired_col=data_columns[i], align_diretion='right')
      colnames(data2) = rev(digitdata@right_aligned_column_names[1:length(data2)])
      #get subset of data if specified
      if (!(is.na(indexes[1]))){
        data = data[indexes, ]
      }
      data = dplyr::bind_rows(data, data2)
    }
  }
  #change columns in correct order
  data = data[rev(digitdata@right_aligned_column_names[1:length(data)])]
  return(data)
}

#' Obtains expected mean conforming to Benford's Law from input \code{data} based on the number of digits in each left-aligned digit place
#'
#' @param data A dataframe of right-aligned digits table, preferably from \code{combine_by_columns}
#' @param Benford_mean A table for Benford mean in each digit place after removing 0 and/or 5 if desired, preferably from \code{get_benford_mean}
#' @inheritParams padding_test
#'
#' @return A list of 4 items:
#' \itemize{
#'   \item \code{expected_mean}: A table of expected mean for the dataset \code{data}
#'   \item \code{final_data}: Cleaned final data to be used for padding test after getting rid of numbers > max_length and numbers < num_digits
#'   \item \code{freq}: The number digits of each length in \code{final_data}
#'   \item \code{freq_table}: The number of left-aligned digit place numbers in each right-aligned digit position in \code{final_data}
#' }
get_expected_mean = function(digitdata, data, Benford_mean, max_length, num_digits){
  if (max_length < num_digits){
    stop('max_length must be as least as large as num_digits')
  }

  indexes_over_max_length = NA
  if (max_length < length(data)){
    #to be removed
    indexes_over_max_length = which(complete.cases(data[(length(data)-max_length):length(data)]) == TRUE)
  }
  #to use, including the ones passes max length
  indexes_qualified = which(complete.cases(data[(length(data)-num_digits+1):length(data)]) == TRUE)

  #final indexes to be used
  indexes_to_use = setdiff(indexes_qualified, indexes_over_max_length)

  #final data to be used
  final_data = data[indexes_to_use, ]

  #count the number digits of each length in this dataframe
  freq = table(rowSums(!(is.na(final_data))))

  #initialize a table to store the number of left-aligned digit place numbers in each right-aligned digit position
  #row means the right alighed digit position
  #column means how many are from nth digit place
  freq_table = data.frame(matrix(0, ncol = num_digits, nrow = max_length))
  colnames(freq_table) = num_digits:1
  rownames(freq_table) = digitdata@left_aligned_column_names[1:max_length]

  #fill out the table in a diagonal fashion
  for (name in names(freq)){
    length = as.integer(name)
    row = length - num_digits + 1
    for (col in 1:num_digits){
      freq_table[row, col] = freq_table[row, col] + freq[name]
      row = row + 1
    }
  }
  #intialize table for expected mean
  expected_mean = data.frame(matrix(nrow = 1, ncol = num_digits))
  colnames(expected_mean) = rev(digitdata@right_aligned_column_names[1:num_digits])

  #get the benford expacted mean for this data set in each digit place aligned right
  for (i in 1:num_digits){
    expected_mean[i] = sum(Benford_mean[1:max_length]*freq_table[,i])/sum(freq_table[,i])
  }
  return(list(expected_mean=expected_mean, final_data=final_data, freq=freq, freq_table=freq_table))
}


#' Obtains the observed mean from input \code{data}
#'
#' @inheritParams get_expected_mean
#' @inheritParams padding_test
#'
#' @return A table of observed mean for the dataset \code{data}
get_observed_mean = function(data, num_digits){
  if (num_digits > length(data)){
    stop('the number of digits desired to evaluate is greater than the max length number in the dataset')
  }
  #the digit places from right we are interested in
  data = data[(length(data)-num_digits+1):length(data)]
  if (!(is.na(omit_05[1]))){
    #remove 0 entries
    data[data == 0] = NA

    if (length(omit_05) == 2){
      #remove 5 entries
      data[data == 5] = NA
    }
  }
  observed_mean = data.frame(t(colMeans(data, na.rm = TRUE))) #do t() such that it is a dataframe
  colnames(observed_mean) = colnames(data)#change col names cuz it is weird due to data.frame()
  return(observed_mean)
}


#' Stimulatse N Benford distribution datasets with matching digit places as the observed dataset. SHOULD I EXPORT THIS???!!!
#'
#' @param freq_table \code{freq_table} item returned from \code{get_expected_mean}
#' @param expected_mean \code{expected_mean} item returned from \code{get_expected_mean}
#' @inheritParams padding_test
#'
#' @return A dataframe for the mean of each stimulated dataset in every digit place
#' @export
Benford_stimulation = function(N, freq_table, expected_mean, contingency_table){

  #sample each digit place from right according to frequency table of our observed dataset
  #initialize the returned table
  stimulated_mean = data.frame(matrix(nrow = 0, ncol = length(expected_mean)))
  colnames(stimulated_mean) = colnames(expected_mean)

  #stimulate n datasets
  for (n in 1:N){
    #initialize the row for this stimulated set
    stimulated_mean[paste('sample', as.character(n)), ] = NA
    for (i in 1:length(freq_table)){
      #the frequency of each left-aligned digits in each right-aligned digit
      freq_of_digit_position = freq_table[[i]]

      #stimulate data
      stimulated_numbers = c()
      for (j in 1:length(freq_of_digit_position)){
        #returns logical(0) if sample 0 number
        size = freq_of_digit_position[j]
        if (size != 0){
          #sample according to the digit place probability and with the size
          stimulated_numbers = c(stimulated_numbers, sample(contingency_table[['Digits']], size = size, replace = TRUE, prob = contingency_table[[paste('Digit Place', as.character(j))]]))
        }
      }
      stimulated_mean[paste('sample', as.character(n)), ][i] = mean(stimulated_numbers)
    }
  }
  return(stimulated_mean)
}


#' Obtains p_values for comparing with Monte Carlo stimulated datasets
#'
#' @param observed_mean \code{observed_mean} returned from \code{get_observed_mean}
#' @param stimulated_mean \code{stimulated_mean} returned from \code{Benford_stimulation}
#'
#' @return A table of p-values for each digit place
get_p_value = function(observed_mean, stimulated_mean){
  p_values = data.frame(matrix(nrow=1, ncol=length(observed_mean)))
  colnames(p_values) = colnames(observed_mean)

  for (i in 1:length(observed_mean)){
    #combine observed and stimulated and find rank of observed
    #first row is observation --> first index
    p_values[i] = rank(-rbind(observed_mean, stimulated_mean)[[i]])[1] / (length(stimulated_mean[,i]) + 1) ####not sure N+1?
  }
  return(p_values)
}

####
#need ADD plot parameter
####

#' Performs padding test vs stimulations of Benford conforming datasets via percentile
#'
#' @param max_length The length of the longest numbers considered. Defaulted to 8.
#' @param num_digits The total number of digits aligned from the right to be analyzed. Defaulted to 5, meaning analyzing digit place 1s to 10ks.
#' @param N The number of Benford conforming datasets to stimulate.
#' @inheritParams all_digits_test
#'
#' @return
#' \itemize{
#'   \item A list with 4 elements
#'   \itemize{
#'     \item \code{expected_mean}: the expected mean by Benford's Law
#'     \item \code{observed_mean}: the mean of the input data
#'     \item \code{diff_in_mean}: the mean difference betweeen observed_mean and expected_mean
#'     \item \code{p_values}: the percentile of the observed dataset among all stimulated datasets in decreasing order
#'   }
#'   \item Plots on \code{diff_in_mean} for each category if \code{plot = TRUE}
#' }
#' @export
#'
#' @examples
#' padding_test(digitdata, contingency_table, data_columns='all')
#' padding_test(digitdata, contingency_table, data_columns=c('col_name1', 'col_name2'), omit_05=NA)
#' padding_test(digitdata, contingency_table, data_columns='all', max_length=7, num_digits=3, omit_05=0)
#' padding_test(digitdata, contingency_table, data_columns='all', N=100, omit_05=NA, break_out='col_name')
padding_test = function(digitdata, contingency_table, data_columns='all', max_length=8, num_digits=5, N=10000, omit_05=c(0,5), break_out=NA){

  #check input
  input_check(digitdata=digitdata, contingency_table=contingency_table, data_columns=data_columns, omit_05=omit_05,
              break_out=break_out, max_length=max_length, num_digits=num_digits, N=N)

  #get benford mean in each digit place
  Benford = get_benford_mean(contingency_table, omit_05)
  Benford_mean = Benford$Benford_mean
  contingency_table = Benford$contingency_table

  ######################################################
  #handle the data_columns = 'all' situation
  data_columns = get_data_columns(digitdata, data_columns)

  #get combined by rows data for all data columns needed
  combined_data = combine_by_columns(digitdata, data_columns, indexes=NA)

  #get expected and observed mean in each digit position
  lst = get_expected_mean(digitdata, combined_data, Benford_mean, max_length, num_digits)
  freq_table=lst$freq_table

  expected_mean = lst$expected_mean
  rownames(expected_mean) = 'all'

  observed_mean = get_observed_mean(lst$final_data, num_digits)
  rownames(observed_mean) = 'all'

  #get the difference in expected and observed mean in each digit position
  diff_in_mean = observed_mean - expected_mean
  rownames(diff_in_mean) = 'all'

  #Monte Carlo Stimulation of N datasets and get mean
  stimulated_mean = Benford_stimulation(N, freq_table, expected_mean, contingency_table)

  #get p values by comparing with stimulation
  p_values = get_p_value(observed_mean, stimulated_mean)
  rownames(p_values) = 'all'
  ######################################################

  #break out by category
  if (!(is.na(break_out))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, break_out) #this is a list since unequal number of entries for each category

    #break by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]

      ######################################################
      #get combined by rows data for all data columns needed
      combined_data_of_category = combine_by_columns(digitdata, data_columns, indexes=indexes_of_category)

      #get expected and observed mean in each digit position
      lst = get_expected_mean(digitdata, combined_data_of_category, Benford_mean, max_length, num_digits)
      freq_table=lst$freq_table

      expected_mean[category_name, ] = lst$expected_mean

      observed_mean[category_name, ] = get_observed_mean(lst$final_data, num_digits)

      #get the difference in expected and observed mean in each digit position
      diff_in_mean[category_name, ]  = observed_mean[category_name, ] - expected_mean[category_name, ]

      #Monte Carlo Stimulation of N datasets and get mean
      stimulated_mean = Benford_stimulation(N, freq_table, expected_mean[category_name, ], contingency_table)

      #get p values by comparing with stimulation
      p_values[category_name, ] = get_p_value(observed_mean[category_name, ], stimulated_mean)
      ######################################################
    }
  }
  return(list(diff_in_mean=diff_in_mean, p_values=p_values, expected_mean=expected_mean, observed_mean=observed_mean))
}

