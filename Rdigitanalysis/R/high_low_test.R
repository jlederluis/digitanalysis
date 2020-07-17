############################################################
#Functions for digit analysis R package
###high to low digit test functions in this file
#Wenjun Chang
#Summer 2020
############################################################


############################################################
#helper function
############################################################

#compute the high low digit binomial test by digit place for desired data columns
#a helper function for the main function
high_low_by_digit_place = function(digitdata, data, high, high_freq_theoratical, skip_first_figit, omit_05){
  #intialize a table for storing total high and low digits counts for each digit place
  high_and_low_total_counts = data.frame(matrix(0, nrow = 2, ncol = digitdata@max))
  #name col and row for debug purpose
  rownames(high_and_low_total_counts) = c('high digit counts', 'low digits counts')
  colnames(high_and_low_total_counts) = digitdata@left_aligned_column_names[1:length(high_and_low_total_counts)]

  #count high low digits in each column
  for (name in colnames(data)){
    for (i in 1:length(digitdata@left_aligned_column_names)){
      if (grepl(digitdata@left_aligned_column_names[i], name, fixed = TRUE)){
        #i is the digit place of this column

        #get frequency of each digit in each digit place
        counts_obs = table(data[name])
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
  if (skip_first_figit){
    p_values = p_values[-1]
  }
  rownames(p_values) = 'p value'
  return(p_values)
}


################main function############
#performs high to low digit tests vs probability of high to low digits by Benford's Law via binomial test
#digitdata is the class object;
#contingency_table is the Benford table
#data_columns are the names of numerical columns of data to be analyzed (defaulted as 'all' to the entire number table)
#high is an array of digits in integer (in fact both char and int should be fine) that specifies the digits that are classified as high digits, defaulted to c(6,7,8,9)
#omit_05 means if we count which of trailing 0 or 5 as rounded
#omit_05 has three options: omit both 0 and 5->c(0,5)/c(5,0); omit only 0->0 or c(0); and omit neither->NA (when no rounding test is performed)
#if analysis by groups is desired, break_out should specify the deisred category to break upon--> we perform a 'year' effect anaylsis instead of a general high low digit test


####
#need ADD plot parameter
####
high_low_test = function(digitdata, contingency_table, data_columns, high=c(6,7,8,9), omit_05=c(0,5), skip_first_figit=TRUE, last_digit_test_included=FALSE, break_out=NA){

  #checkings
  if (length(omit_05) == 1){
    ###check omit only 5, which is not allowed
    if (!(is.na(omit_05)) && (omit_05 == 5)){
      stop('cannot omit only 5 without also omitting 0 first')
    }
  }

  #get table for the theoratical high to low freqency in each digit place
  #if omit_05 in high, then should throw error...no way should it be omiited and counted as highh digit
  #drop X and Digits column of contingency table
  high_freq_theoratical = contingency_table[!(colnames(contingency_table) %in% c('X', 'Digits'))]
  rownames(high_freq_theoratical) = 0:9

  #drop 0 and/or 5
  if (!(is.na(omit_05[1]))){
    high_freq_theoratical = high_freq_theoratical[-(omit_05+1), ]  ### +1 since omit_05 is digits begins with 0, while indexes begins with 1
  }

  ###normalize the columns after (if) dropping 0 and/or 5
  for (name in colnames(high_freq_theoratical)){
    #renormialize
    high_freq_theoratical[name] = high_freq_theoratical[name] / sum(high_freq_theoratical[name])
  }

  #get the frequency for high digits in each digit place
  high_freq_theoratical = t(data.frame(colSums(high_freq_theoratical[as.character(high), ])))
  rownames(high_freq_theoratical) = 'high digits freq'


  #get the data columns desired
  lst = grab_desired_aligned_columns(digitdata, data_columns, skip_first_figit=skip_first_figit, last_digit_test_included=last_digit_test_included, align_direction='left')
  data = lst$digits_table
  digitdata = lst$digitdata

  #perform a standard high low test
  if (is.na(break_out)){
    p_values = high_low_by_digit_place(digitdata, data, high, high_freq_theoratical, skip_first_figit, omit_05)
    return(p_values)
  }

  #perform a 'year effect' high low test break by category
  else {
    #initlaize a list to be returned
    output = list()

    #get indexes for each category
    indexes_of_categories = break_by_category(data=digitdata@cleaned, break_out=break_out) #this is a list since unequal number of entries for each category

    #break by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]
      data_of_category = data.frame(data[indexes_of_category, ])
      #when do data.frame.... col names changes from A BC to A.BC
      colnames(data_of_category) = gsub("."," ",colnames(data_of_category), fixed=TRUE)

      #get p_values for this category ('year')
      p_values = high_low_by_digit_place(digitdata, data_of_category, high, high_freq_theoratical, skip_first_figit, omit_05)

      #update returning list
      output[[category_name]] = p_values
    }
    return(output)
  }
}

