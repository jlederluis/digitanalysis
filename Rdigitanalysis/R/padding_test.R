############################################################
#Functions for digit analysis R package
###padding test functions in this file
#Wenjun Chang
#Summer 2020
############################################################



############################################################
#helper functions
############################################################



##################!!!!!!!!!!!!!!!!might also use else where
#get Benford mean in each digit place after specifying omitting 0 and 5 or not
get_benford_mean = function(contingency_table, omit_05){
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
    #renormialize
    contingency_table[name] = contingency_table[name] / sum(contingency_table[name])
    #get mean
    Benford_mean[name] = sum(contingency_table[name] * contingency_table['Digits'])
  }
  return(list(Benford_mean=Benford_mean, contingency_table=contingency_table))
}

##################!!!!!!!!!!!!!!!!might also use else where
#return the merged rows of all data columns aligned-right digits
combine_by_columns = function(digitdata, data_columns, indexes=NA){
  data = single_column_aligned(digitdata, desired_col=data_columns[1], align_diretion='right')
  if (!(is.na(indexes[1]))){
    data = data[indexes, ]
  }
  colnames(data) = rev(digitdata@right_aligned_column_names[1:length(data)])

  if (length(data_columns) > 1){
    for (i in 2:length(data_columns)){
      data2 = single_column_aligned(digitdata, desired_col=data_columns[i], align_diretion='right')
      colnames(data2) = rev(digitdata@right_aligned_column_names[1:length(data2)])
      data = dplyr::bind_rows(data, data2)
    }
  }

  #change columns in correct order
  data = data[rev(digitdata@right_aligned_column_names[1:length(data)])]

  return(data)
}

get_expected_mean = function(digitdata, data, Benford_mean, max_length, num_digits){
  if (max_length < num_digits){
    stop('max_length must be as least as large as num_digits')
  }

  indexes_over_max_length = NA
  if (max_length < length(data)){
    #to be removed
    indexes_over_max_length = which(complete.cases(data[(length(data)-max_length):length(data)]) == TRUE)
  }

  #to use
  indexes_qualified = which(complete.cases(data[(length(data)-num_digits+1):length(data)]) == TRUE)

  #final indexes to be used
  indexes_to_use = setdiff(indexes_qualified, indexes_over_max_length)

  #final data to be used
  final_data = data[indexes_to_use, ]

  #count the number digits of each length in this dataframe
  freq = table(rowSums(!(is.na(final_data))))

  #initialize a table to store the number of digit place numbers in each right-aligned digit position
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

#get the observed mean from data
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


#Stimulate N Benford distribution datasets with matchig digit places as the observed
#return the mean of each of the N stimulated dataset
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


#get p_value for comparing with MC stimulated datasets
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


################main function############
#performs padding test vs stimulations of Benford conforming datasets via percentile
#digitdata is the class object;
#contingency_table is the Benford table
#data_columns are the names of numerical columns of data to be analyzed (defaulted as 'all' to the entire number table)
#max length is the length of the longest numbers considered, default to 8, which is the length of the pre-computed Benford table
#num_digits is the total number of digits aligned from the right to be analyzed, defaulted to 5, so ananlying 1s to 10ks digit place
#N is the number of Benford conforming datasets to stimulate
#omit_05 means if we count which of trailing 0 or 5 as rounded
#omit_05 has three options: omit both 0 and 5->c(0,5)/c(5,0); omit only 0->0 or c(0); and omit neither->NA (when no rounding test is performed)
#if analysis by groups is desired, break_out should specify the deisred category to break upon

#return a list object, where
#expected_mean is the expected mean by Benford's Law
#observed_mean is the mean of the input data
#diff_in_mean is the mean difference betweeen observed_mean and expected_mean
#p_values is the percentile of the observed dataset among all stimulated datasets, in decreasing order


####
#need ADD plot parameter
####
padding_test = function(digitdata, contingency_table, data_columns='all', max_length=8, num_digits=5, N=10000, omit_05=c(0,5), break_out=NA){

  #checkings
  if (length(omit_05) == 1){
    ###check omit only 5, which is not allowed
    if (!(is.na(omit_05)) && (omit_05 == 5)){
      stop('cannot omit only 5 without also omitting 0 first')
    }
  }

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

