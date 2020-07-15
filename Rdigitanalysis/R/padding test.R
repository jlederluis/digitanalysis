############################################################
#Functions for digit analysis R package
###padding test functions in this file
#Wenjun Chang
#Summer 2020
############################################################

############################################################
#helper function
############################################################


#############prelim############
#clear workspace
rm(list = ls())
#free up R memory
gc()
#force numerical representation rather than scientific
#options(scipen = 999)
options(scipen = 1)
options(digits = 2)
##############################

#load data input functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\data input functions.R')

#load functions for computing Benford table
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\Benford table functions.R')

#load helper functions for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all digit test helper functions.R')

#load chi square test GOF functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\chi square goodness of fit functions.R')

#load main function for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all digit test main function.R')

#load all functions for digit pair test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\digit pair test.R')

#load all functions for rounding test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\rounding test.R')

#load all functions for repeat test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\repeat test.R')

#load all functions for high low test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\high low test.R')



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
    Benford_mean[name] = sum(contingency_table[name] * contingency_table['Digits'] / sum(contingency_table[name]))
  }
  return(Benford_mean)
}


#return the merged rows of all data columns aligned-right digits
combine_by_columns = function(digitdata, data_columns){
  data = single_column_aligned(digitdata, desired_col=data_columns[1], align_diretion='right')
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

  return(list(expected_mean=expected_mean, final_data=final_data))
}


#get the observed mean from data
get_observed_mean = function(data, num_digits){
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
  observed_mean = colMeans(data, na.rm = TRUE)
  return(observed_mean)
}

#main fucntion
padding_test = function(digitdata, contingency_table, data_columns, max_length, num_digits, omit_05){

  #checkings
  if (length(omit_05) == 1){
    ###check omit only 5, which is not allowed
    if (!(is.na(omit_05)) && (omit_05 == 5)){
      stop('cannot omit only 5 without also omitting 0 first')
    }
  }

  #get benford mean in each digit place
  Benford_mean = get_benford_mean(contingency_table, omit_05)
  #get combined by rows data for all data columns needed
  combined_data = combine_by_columns(digitdata, data_columns)

  #get expected and observed mean in each digit position
  lst = get_expected_mean(digitdata, combined_data, Benford_mean, max_length, num_digits)
  expected_mean = lst$expected_mean
  observed_mean = get_observed_mean(lst$final_data, num_digits)

  #get the difference in expected and observed mean in each digit position
  diff_in_mean = observed_mean - expected_mean
  return(diff_in_mean)

  #need to do MC....
}


#############################################################
#############try it with given data##########################
#############################################################

#test data input and benford table functions
#load data input functions
data_columns = c("ALEXP","BENTOT", "BENM", "BENF")
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'

DigitData = make_class(filepath = fp, col_analyzing = data_columns)

contingency_table = load_Benford_table('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')
contingency_table

omit_05=c(0,5)

data_columns = c("ALEXP","BENTOT", "BENM", "BENF")
max_length = 7
num_digits = 5


padding_test(DigitData, contingency_table, data_columns, max_length, num_digits, omit_05)


























Benford_mean = get_benford_mean(contingency_table, omit_05)
Benford_mean


combined_data = combine_by_columns(DigitData, data_columns)




max_length = 7
num_digits = 5


lst = get_expected_mean(DigitData, combined_data, Benford_mean, max_length, num_digits)
expected_mean = lst$expected_mean
observed_mean = get_observed_mean(lst$final_data, num_digits)

expected_mean
observed_mean

diff_in_mean = observed_mean - expected_mean
diff_in_mean







##get the observed mean now
data_to_use = data[indexes_to_use, ]
data_to_use = data_to_use[(length(data_to_use)-num_digits+1):length(data_to_use)]
data_to_use[data_to_use == 0] = NA
data_to_use[data_to_use == 5] = NA
data_to_use
observed_mean = colMeans(data_to_use, na.rm = TRUE)
observed_mean


diff_in_mean = observed_mean - expected_mean
diff_in_mean

data = single_column_aligned(DigitData, desired_col='ALEXP', align_diretion='right')
colnames(data) = rev(DigitData@right_aligned_column_names[1:length(data)])
data2 = single_column_aligned(DigitData, desired_col='BENTOT', align_diretion='right')
colnames(data2) = rev(DigitData@right_aligned_column_names[1:length(data2)])
a= dplyr::bind_rows(data2, data)
a[rev(DigitData@right_aligned_column_names[1:length(a)])]






