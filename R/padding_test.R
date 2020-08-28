############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# Padding Test functions in this file
############################################################

#' Obtains Benford mean in each digit place after specifying omitting 0 and 5 or not
#'
#' @inheritParams padding_test
#'
#' @return A table for Benford mean in each digit place after removing 0 and/or 5 if desired
#' @export
get_benford_mean = function(contingency_table, omit_05=NA){
  #create a table for the mean of benford distribution in each digit place
  names = colnames(contingency_table)[!(colnames(contingency_table) %in% c("X", "Digits"))]
  Benford_mean = data.frame(matrix(nrow = 1, ncol = length(names)))
  colnames(Benford_mean) = names
  rownames(Benford_mean) = 'mean'

  #modify contingency table with omit_05 ### +1 since omit_05 is digits begins with 0, while indexes begins with 1
  if (!(is.na(omit_05[1]))){
    contingency_table = contingency_table[-(omit_05+1), ]
  }
  #/ sum(contingency_table[name] to normailize if p does not sum to 1
  for (name in colnames(Benford_mean)){
    #renormalize
    contingency_table[name] = contingency_table[name] / sum(contingency_table[name])
    #get mean
    Benford_mean[name] = sum(contingency_table[name] * as.numeric(rownames(contingency_table)))
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
  data = single_column_aligned(digitdata, desired_col=data_columns[1], align_direction='right')
  #get subset of data if specified
  if (!(is.na(indexes[1]))){
    data = data[indexes, ]
  }
  colnames(data) = rev(digitdata@right_aligned_column_names[1:length(data)])

  if (length(data_columns) > 1){
    for (i in 2:length(data_columns)){
      data2 = single_column_aligned(digitdata, desired_col=data_columns[i], align_direction='right')
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
#'   \item \code{freq_table}: The number of left-aligned digit place numbers in each right-aligned digit position in \code{data}
#' }
get_expected_mean = function(digitdata, data, Benford_mean, max_length, num_digits, omit_05){
  if (max_length < num_digits){
    stop('max_length must be as least as large as num_digits')
  }
  #initialize a table to store the number of left-aligned digit place numbers in each right-aligned digit position
  #row means the right alighed digit position
  #column means how many are from nth digit place
  freq_table = data.frame(matrix(0, ncol = num_digits, nrow = max_length))
  colnames(freq_table) = num_digits:1
  rownames(freq_table) = digitdata@left_aligned_column_names[1:max_length]

  #do it for each length each time
  for (curr_length in num_digits:max_length){
    #get data of current length
    #print(rowSums(!(is.na(data))))

    data_of_curr_length = data[which(rowSums(!(is.na(data))) == curr_length), ]

    if (nrow(data_of_curr_length) == 0) next # do not proceed if it is empty...throw error on some computers!!!!!!!!!!!!!!!!!!!!!!!!

    #ensure only num_digits columns from right
    data_of_curr_length = data_of_curr_length[(ncol(data_of_curr_length)-num_digits+1):ncol(data_of_curr_length)]

    #print(head(data_of_curr_length))
    #remove 0 and 5 if specified
    if (!(is.na(omit_05[1]))){
      #omit 0
      data_of_curr_length[data_of_curr_length == 0] = NA
      if (length(omit_05) == 2){
        #also omit 5
        data_of_curr_length[data_of_curr_length == 5] = NA
      }
    }
    #count the number of entries in each column and update to freq_table
    for (i in 1:ncol(data_of_curr_length)){
      nums = length(which(!(is.na(data_of_curr_length[i]))))
      #col index: ith position from the right --> i = num_digits:1 --> reverse order
      #row index: in terms of curr_length, i is the (curr_length - num_digits + i)th digit place
      freq_table[(curr_length - num_digits + i), i] = nums
    }
  }
  #intialize table for expected mean
  expected_mean = data.frame(matrix(nrow = 1, ncol = num_digits))
  colnames(expected_mean) = rev(digitdata@right_aligned_column_names[1:num_digits])

  #get the benford expacted mean for this data set in each digit place aligned right
  for (i in 1:num_digits){
    expected_mean[i] = sum(Benford_mean[1:max_length]*freq_table[,i])/sum(freq_table[,i])
  }
  return(list(expected_mean=expected_mean, freq_table=freq_table))
  # indexes_over_max_length = NA
  # if (max_length < length(data)){
  #   #to be removed
  #   indexes_over_max_length = which(complete.cases(data[(length(data)-max_length):length(data)]) == TRUE)
  # }
  # #to use, including the ones passes max length
  # indexes_qualified = which(complete.cases(data[(length(data)-num_digits+1):length(data)]) == TRUE)
  #
  # #final indexes to be used
  # indexes_to_use = setdiff(indexes_qualified, indexes_over_max_length)
  #
  # #final data to be used
  # final_data = data[indexes_to_use, ]
  #
  # #remove 0 and 5 in finalm data if specified
  # if (!(is.na(omit_05[1]))){
  #   #omit 0
  #   final_data[final_data == 0] = NA
  #   if (length(omit_05) == 2){
  #     #also omit 5
  #     final_data[final_data == 5] = NA
  #   }
  # }
  # #count the number digits of each length in this dataframe
  # freq = table(rowSums(!(is.na(final_data))))
  # print(freq)
  #
  # #initialize a table to store the number of left-aligned digit place numbers in each right-aligned digit position
  # #row means the right alighed digit position
  # #column means how many are from nth digit place
  # freq_table = data.frame(matrix(0, ncol = num_digits, nrow = max_length))
  # colnames(freq_table) = num_digits:1
  # rownames(freq_table) = digitdata@left_aligned_column_names[1:max_length]
  #
  # #fill out the table in a diagonal fashion
  # for (name in names(freq)){
  #   length = as.integer(name)
  #   row = length - num_digits + 1
  #   for (col in 1:num_digits){
  #     freq_table[row, col] = freq_table[row, col] + freq[name]
  #     row = row + 1
  #   }
  # }
  # #intialize table for expected mean
  # expected_mean = data.frame(matrix(nrow = 1, ncol = num_digits))
  # colnames(expected_mean) = rev(digitdata@right_aligned_column_names[1:num_digits])
  #
  # #get the benford expacted mean for this data set in each digit place aligned right
  # for (i in 1:num_digits){
  #   expected_mean[i] = sum(Benford_mean[1:max_length]*freq_table[,i])/sum(freq_table[,i])
  # }
  # print(freq)
  # print(freq_table)
  # return(list(expected_mean=expected_mean, final_data=final_data, freq=freq, freq_table=freq_table))
}


#' Obtains the observed mean from input \code{data}
#'
#' @inheritParams get_expected_mean
#' @inheritParams padding_test
#'
#' @return A table of observed mean for the dataset \code{data}
get_observed_mean = function(data, num_digits, max_length, omit_05){
  if (num_digits > length(data)){
    stop('the number of digits desired to evaluate is greater than the max length number in the dataset')
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
  data = data[indexes_to_use, ]

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


#' Stimulates N Benford distribution datasets with matching digit places as the observed dataset. SHOULD I EXPORT THIS???!!!
#'
#' @param freq_table \code{freq_table} item returned from \code{get_expected_mean}
#' @param expected_mean \code{expected_mean} item returned from \code{get_expected_mean}
#' @inheritParams padding_test
#'
#' @return A dataframe for the mean of each simulated dataset in every digit place
#' @export
Benford_simulation = function(N, freq_table, expected_mean, contingency_table){
  #sample each digit place from right according to frequency table of our observed dataset
  #initialize the returned table
  simulated_mean = data.frame(matrix(nrow = 0, ncol = length(expected_mean)))
  colnames(simulated_mean) = colnames(expected_mean)

  # #debug
  # times = c()
  #simulate n datasets
  # start = proc.time()
  for (n in 1:N){
    #printing for user
    if (n %% 5000 == 0){
      print(paste("Simulated", n, 'datasets...'))
      # print(paste('Current memory:', memory.size()))
      # print(paste('Current dataset total digits:', sum(freq_table)))
      # print(paste('Current time:', (proc.time()-start)[[3]], 'seconds'))
      # print("")
      # times = c(times, (proc.time()-start)[[3]])
    }

    #initialize the row for this simulated set
    simulated_mean[paste('sample', as.character(n)), ] = NA

    for (i in 1:length(freq_table)){
      #the frequency of each left-aligned digits in each right-aligned digit
      freq_of_digit_position = freq_table[[i]]
      #simulate data
      simulated_numbers = c()
      for (j in 1:length(freq_of_digit_position)){
        #returns logical(0) if sample 0 number
        size = freq_of_digit_position[j]
        if (size != 0){
          #sample according to the digit place probability and with the size
          simulated_numbers = c(simulated_numbers, sample(as.numeric(rownames(contingency_table)), size = size, replace = TRUE, prob = contingency_table[[paste('Digit Place', as.character(j))]]))
        }
      }
      simulated_mean[paste('sample', as.character(n)), ][i] = mean(simulated_numbers)
    }
  }
  #return(times)
  return(simulated_mean)
}


#' Obtains p_values for comparing with Monte Carlo simulated datasets
#'
#' @param observed_mean \code{observed_mean} returned from \code{get_observed_mean}
#' @param simulated_mean \code{simulated_mean} returned from \code{Benford_simulation}
#' @param diff_in_mean The difference in mean of \code{observed_mean} - \code{simulated_mean}
#'
#' @return A table of p-values for each digit place
get_p_value = function(observed_mean, simulated_mean, diff_in_mean){

  p_values = data.frame(matrix(nrow=1, ncol=length(observed_mean)))
  colnames(p_values) = colnames(observed_mean)
  combined_mean_data = rbind(observed_mean, simulated_mean)

  for (i in 1:length(observed_mean)){
    #combine observed and simulated and find rank of observed
    if (is.na(diff_in_mean[i])){
      #this is stupid...some category has no data
      p_values[i] = NA
    }
    else {
      #diff_in_mean positive, look at how many simulations larger than it
      if (diff_in_mean[i] > 0){
        #first row is observation --> first index
        p_values[i] = (rank(-combined_mean_data[[i]])[1] - 1) / length(simulated_mean[,i])
      }
      #diff_in_mean negative, look at how many simulations smaller than it
      else {
        #first row is observation --> first index
        p_values[i] = (rank(combined_mean_data[[i]])[1] - 1) / length(simulated_mean[,i])
      }
      #either largest or smallest number, give it p-value 1/N since it cannot be 0
      if (p_values[i] == 0){
        p_values[i] = 1/length(simulated_mean[,i])
      }
    }
    p_values[i] = format_p_values(p_values[i])
  }
  return(p_values)
}


#' Perform a single padding test. Helper function for \code{padding_test}.
#'
#' @inheritParams padding_test
#'
#' @return A list of padding test results for input data from \code{digitdata}.
single_padding_test = function(digitdata, contingency_table, data_columns, max_length, num_digits, N, omit_05, category, category_grouping, simulate){

  #get benford mean in each digit place
  Benford = get_benford_mean(contingency_table, omit_05)
  Benford_mean = Benford$Benford_mean
  contingency_table = Benford$contingency_table

  ######################################################
  #handle the data_columns = 'all' situation
  data_columns = get_data_columns(digitdata, data_columns)

  #get combined by rows data for all data columns needed
  combined_data = combine_by_columns(digitdata, data_columns, indexes=NA)

  #printing for user
  if (simulate){
    print(paste("Simulating sub-dataset:", 'All'))
  }

  #get expected and observed mean in each digit position
  lst = get_expected_mean(digitdata, combined_data, Benford_mean, max_length, num_digits, omit_05)
  freq_table=lst$freq_table

  expected_mean = lst$expected_mean
  rownames(expected_mean) = 'All'

  observed_mean = get_observed_mean(combined_data, num_digits, max_length, omit_05)
  rownames(observed_mean) = 'All'

  #get the difference in expected and observed mean in each digit position
  diff_in_mean = observed_mean - expected_mean
  rownames(diff_in_mean) = 'All'

  p_values = 'No p-values since simulate=FALSE'
  if (simulate){
    #Monte Carlo simulation of N datasets and get mean
    simulated_mean = Benford_simulation(N, freq_table, expected_mean, contingency_table)

    #get p values by comparing with simulation
    p_values = get_p_value(observed_mean, simulated_mean, diff_in_mean)
    rownames(p_values) = 'All'
  }
  ######################################################

  #break out by category
  if (!(is.na(category))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, category, category_grouping) #this is a list since unequal number of entries for each category

    #break by category for all
    for (category_name in names(indexes_of_categories)){

      #print(category_name)



      #printing for user
      if (simulate){
        print(paste("Simulating sub-dataset:", category_name))
      }

      #get the index of category containing multiple groups
      indexes_of_category = indexes_of_categories[[category_name]]

      ######################################################
      #get combined by rows data for all data columns needed
      combined_data_of_category = combine_by_columns(digitdata, data_columns, indexes=indexes_of_category)

      #get expected and observed mean in each digit position
      lst = get_expected_mean(digitdata, combined_data_of_category, Benford_mean, max_length, num_digits, omit_05)
      freq_table=lst$freq_table
      expected_mean[category_name, ] = lst$expected_mean
      observed_mean[category_name, ] = get_observed_mean(combined_data_of_category, num_digits, max_length, omit_05)

      #get the difference in expected and observed mean in each digit position
      diff_in_mean[category_name, ]  = observed_mean[category_name, ] - expected_mean[category_name, ]
      if (simulate){
        #Monte Carlo Simulation of N datasets and get mean
        simulated_mean_in_category = Benford_simulation(N, freq_table, expected_mean[category_name, ], contingency_table)

        #get p values by comparing with simulation
        p_values[category_name, ] = get_p_value(observed_mean[category_name, ], simulated_mean_in_category, diff_in_mean[category_name, ])
      }
      ######################################################
    }
    if (!(TRUE %in% grepl("\\D", rownames(diff_in_mean)[-1]))){
      #then it is numeric..sort them
      ordered_rows = c('All', as.character(sort(as.numeric(rownames(diff_in_mean)[-1]))))
      if (p_values != "No p-values since simulate=FALSE"){
        p_values = p_values[ordered_rows, ]
      }
      diff_in_mean = diff_in_mean[ordered_rows, ]
    }
  }
  #remove NA rows if exist
  diff_in_mean = diff_in_mean[rowSums(is.na(diff_in_mean)) != ncol(diff_in_mean), ]
  return(list(diff_in_mean=diff_in_mean, p_values=p_values))#, expected_mean=expected_mean, observed_mean=observed_mean))
}


#' Performs padding test vs simulations of Benford conforming datasets via percentile
#'
#' @param max_length The length of the longest numbers considered. Defaulted to 8.
#' @param num_digits The total number of digits aligned from the right to be analyzed. Defaulted to 5, meaning analyzing digit place 1s to 10ks.
#' @param N The number of Benford conforming datasets to simulate.
#' \itemize{
#'   \item 2400 seconds for N=10,000; data dimension = 4000 x 5 total digits.
#' }
#' @param simulate TRUE or FALSE: If TRUE, will stimulate the datasets and generate p-value. If FALSE, only produces \code{diff_in_mean} and plots.
#' Overwrites \code{N}.
#' @inheritParams all_digits_test
#' @inheritParams sector_test
#'
#' @return
#' \itemize{
#'   \item A list with 5 elements
#'   \itemize{
#'     \item \code{expected_mean}: the expected mean by Benford's Law
#'     \item \code{observed_mean}: the mean of the input data
#'     \item \code{diff_in_mean}: the mean difference between observed_mean and expected_mean
#'     \item \code{p_values}: the percentile of the observed dataset among all simulated datasets in decreasing order
#'     \item \code{plot}: one ggplot instance per \code{break_out}
#'   }
#'   \item Plots on \code{diff_in_mean} for each category displayed if \code{plot = TRUE or 'Save'}
#' }
#' @export
#'
#' @examples
#' padding_test(digitdata, omit_05=c(0,5), simulate=FALSE)
#' padding_test(digitdata, data_columns=c('col_name1', 'col_name2'), break_out='col_name')
#' padding_test(digitdata, N=100, break_out='col_name', distribution='uniform', plot='Save')
#' padding_test(digitdata, max_length=10, num_digits=3, omit_05=0, break_out='col_name', category='category_name')
padding_test = function(digitdata, data_columns='all', max_length=8, num_digits=5, N=10000, simulate=TRUE, omit_05=NA,
                        break_out=NA, break_out_grouping=NA, category=NA, category_grouping=NA, distribution='Benford',
                        contingency_table=NA, suppress_first_division_plots=NA, plot=TRUE){
  #check input
  input_check(digitdata=digitdata, contingency_table=contingency_table, data_columns=data_columns, omit_05=omit_05,
              break_out=break_out, break_out_grouping=break_out_grouping, max_length=max_length, num_digits=num_digits,
              N=N, category=category, category_grouping=category_grouping, plot=plot, simulate=simulate,
              suppress_first_division_plots=suppress_first_division_plots)

  #deal with contingency table and distribution situation
  if (TRUE %in% ((is.na(contingency_table)))){
    #if contingency_table is not passed in, use distribution
    if (tolower(distribution) == 'benford'){
      #data("benford_table")
      contingency_table = digitanalysis::benford_table
    }
    else if (tolower(distribution) == 'uniform'){
      #data("uniform_table")
      contingency_table = digitanalysis::uniform_table
    }
    else {
      stop('contingency_table is invalid, and distribution is not one of "benford" or "uniform"!')
    }
  }

  # p_values =
  # #list of results from all break out category to be returned
  # padding_test_results = list()

  #printing for user
  if (simulate){
    print(paste("Simulating N =", N))
    print(paste("Simulating dataset:", 'All'))
  }

  #perform padding test on all data
  result = single_padding_test(digitdata, contingency_table, data_columns, max_length, num_digits, N, omit_05, category, category_grouping, simulate)
  #return(result)

  #p values table to return
  p_values_table = list(All=result$p_values)
  #diff in mean table to return
  diff_in_mean_table = list(All=result$diff_in_mean)
  #plots to return
  plots = list()

  if (plot != FALSE){
    #2D histogram
    padding_plot = NA
    subtitle = ''
    if (!is.na(break_out)){
      subtitle = paste('All ', break_out, sep='')
    }
    if (is.na(category)){
      padding_plot = hist_2D(result$diff_in_mean, data_style='row', xlab='Digit Place', ylab='Deviation from Mean', title=paste('Padding Test \n', subtitle, sep=''))
    }
    #Multi-variable 2D histogram
    else {
      padding_plot = hist_2D_variables(result$diff_in_mean, data_style='row', xlab='Digit Place', ylab='Deviation from Mean', title=paste('Padding Test \n', subtitle, sep=''))
    }
    plots[['All']] = padding_plot
    if (plot == TRUE){
      dev.new()
      print(padding_plot)
    }
  }

  #trivial plotting arg
  if (suppress_first_division_plots){
    plot = 'Save' #dont display plot for break out
  }
  #perform padding test on all break out categories
  if (!(is.na(break_out))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, break_out, break_out_grouping) #this is a list since unequal number of entries for each category

    #break by category for all
    for (break_out_name in names(indexes_of_categories)){


      #print(break_out_name)

      #printing for user
      if (simulate){
        print(paste("Simulating dataset:", break_out_name))
      }

      indexes_of_category = indexes_of_categories[[break_out_name]]

      #create a digitdata class for this category
      digitdata_of_category = make_sub_digitdata(digitdata=digitdata, indexes=indexes_of_category)

      #perform padding test on this category
      result_of_category = single_padding_test(digitdata_of_category, contingency_table, data_columns, max_length, num_digits, N, omit_05, category, category_grouping, simulate)

      #update return tables
      p_values_table[[break_out_name]] = result_of_category$p_values
      #diff in mean table to return
      diff_in_mean_table[[break_out_name]] = result_of_category$diff_in_mean

      if (plot != FALSE){
        padding_plot = NA
        #2D histogram
        if (is.na(category)){
          padding_plot = hist_2D(result_of_category$diff_in_mean, data_style='row', xlab='Digit Place', ylab='Deviation from Mean', title=paste('Padding Test \n', 'Broken out by ', break_out_name, sep=''))
        }
        #Multi-variable 2D histogram
        else {
          padding_plot = hist_2D_variables(result_of_category$diff_in_mean, data_style='row', xlab='Digit Place', ylab='Deviation from Mean', title=paste('Padding Test \n', 'Broken out by ', break_out_name,  sep=''))
        }
        plots[[break_out_name]] = padding_plot

        if (plot == TRUE){
          dev.new()
          print(padding_plot)
        }
      }
    }
  }
  sample_size = 'No sample size since simulate=FALSE'
  if (simulate){
    print('Complete')
    print(paste('Ignore warning:', "In mean.default(simulated_numbers): argument is not numeric or logical: returning NA"))
    sample_size = N
  }
  print(paste('Minimum possible p-value =', 1/N))
  return(list(p_values=p_values_table, diff_in_mean=diff_in_mean_table, sample_size=sample_size, plots=plots))
}

