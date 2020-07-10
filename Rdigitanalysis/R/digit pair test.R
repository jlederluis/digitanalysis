############################################################
#Functions for digit analysis R package
###digit pair test functions in this file
#Wenjun Chang
#Summer 2020
############################################################


############################################################
#some helper functions
############################################################


#find the frequency of terminal digit pairs occuring in the data being analzyed
#return a vector of [# pairs, # not pairs]
counts_observed = function(digitdata, data_columns, omit_05, min_length, last_digit_test_included, indexes=NA){
  occurances = data.frame(matrix(nrow = 0, ncol = 2))

  for (desired_col in data_columns){
    digit_pair_table = single_column_aligned(digitdata, desired_col, align_diretion='right')

    #omit last digit if last digit test is included
    if (last_digit_test_included){
      digit_pair_table = digit_pair_table[-ncol(digit_pair_table)]
    }

    #for break out, we separate into categories by indexing
    if (!(is.na(indexes[1]))){ ####might have bug here...did [1] to get rid of warnings
      digit_pair_table = digit_pair_table[indexes, ]
    }

    #remove all numbers that has length less than min_length

    #only the last three digits
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
  occurances = as.numeric(paste(occurances[,1], occurances[,2], sep=''))

  pairs = c('00', '11', '22', '33', '44', '55', '66', '77', '88', '99')
  if (!(is.na(omit_05[1]))){
    #we omit 00 as part of digit pair
    pairs = pairs[-1]
  }

  #find the counts for all digit pairs
  counts = table(occurances)
  counts = sum(counts[pairs[pairs %in% names(counts)]])

  # #derive the frequency
  # freq_pairs = counts / sum(occurances)

  return(c(counts, sum(occurances) - counts))
}

#get the theoratical frequency of terminal digit pair frequency
freq_true = function(omit_05){
  total = 100 #100 combinations possible
  pairs = 10 #10 pairs possible
  if (length(omit_05) == 1){
    if (!(is.na(omit_05))){
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


################main function############
#performs terminal digit pair binomial test vs uniform distribution (Benford’s Law)
#digitdata is the class object;
#data_columns are the names of numerical columns of data to be analyzed (defaulted as 'all' to the entire number table)
#omit_05 has three options: omit both 0 and 5->c(0,5)/c(5,0); omit only 0->0 or c(0); and omit neither->NA (when no rounding test is performed)
#if analysis by groups is desired, break_out should specify the deisred category to break upon
#distribution can be 'Benford' or 'Uniform' or more ???
#if last_digit_test_included is true, will omit last digit before analysis, since we don't want tests to overlap


####
#need ADD distribution and plot parameter
####

digit_pairs_test = function(digitdata, data_columns, omit_05=c(0,5), min_length=3, last_digit_test_included=FALSE, break_out=NA){

  #checkings
  if (length(omit_05) == 1){
    ###check omit only 5, which is not allowed
    if (!(is.na(omit_05)) && (omit_05 == 5)){
      stop('cannot omit only 5 without also omitting 0 first')
    }
  }

  if (is.na(min_length)){
    stop('min length must be an integer >= 2')
  }
  else if (min_length < 2){
    stop('min length must be an integer >= 2')
  }



  #get the theoratical frequency based on Benford's Law --> Uniform Distribution
  p = freq_true(omit_05)

  #get the observed counts for number of terminal digit pairs
  counts = counts_observed(digitdata, data_columns, omit_05, min_length, last_digit_test_included)

  #get p_value from binomial test
  p_value = binom.test(counts, p)$p.value

  #a dataframe of p values to return
  p_values = data.frame(all=p_value)

  if (!(is.na(break_out))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, break_out) #this is a list since unequal number of entries for each category

    #breeak by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]
      counts_in_category = counts_observed(digitdata, data_columns, omit_05, min_length, last_digit_test_included, indexes_of_category)
      p_value_in_category = binom.test(counts_in_category, p)$p.value

      p_values[category_name] = p_value_in_category
    }
  }
  return(p_values)
}


