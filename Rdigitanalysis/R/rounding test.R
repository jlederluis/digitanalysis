# ############################################################
# #Functions for digit analysis R package
# ###rounding test functions in this file
# #Wenjun Chang
# #Summer 2020
# ############################################################


############################################################
#helper function
############################################################


#compute the percent of rounded digits in a given data frame
#removed all 0 entries before begin
#omit_05 means if we count which of trailing 0 or 5 as rounded
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

################main function############
#performs terminal digit pair binomial test vs uniform distribution (Benford’s Law)
#digitdata is the class object;
#data_columns are the names of numerical columns of data to be analyzed (defaulted as 'all' to the entire number table)
#omit_05 means if we count which of trailing 0 or 5 as rounded
#omit_05 has three options: omit both 0 and 5->c(0,5)/c(5,0); omit only 0->0 or c(0); and omit neither->NA (when no rounding test is performed)
#if analysis by groups is desired, break_out should specify the deisred category to break upon


####
#need ADD plot parameter
####
rounding_test = function(digitdata, data_columns, omit_05=c(0,5), break_out=NA){

  #checkings
  if (length(omit_05) == 1){
    ###check omit only 5, which is not allowed
    if (!(is.na(omit_05)) && (omit_05 == 5)){
      stop('cannot omit only 5 without also omitting 0 first')
    }
    else if (is.na(omit_05)){
      stop('neither trailing 0 or 5 is counted as rounded digits')
    }
  }

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

    #breeak by category for all
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

