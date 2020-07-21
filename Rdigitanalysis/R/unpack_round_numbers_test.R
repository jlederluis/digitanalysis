############################################################
#Functions for digit analysis R package
###unpack round numbers test functions in this file
#Wenjun Chang
#Summer 2020
############################################################


############################################################
#unpack round numbers test
############################################################


#create two copies of digitdata:
#one with only round entries in the specified numeric column
#the other with only unround entries in the specified numeric column
get_round_unround_digitdata = function(digitdata, unpacking_rounding_column){
  #get the round indexes
  round_numbers_indexes = unpacking_round_number_split(digitdata, unpacking_rounding_column)

  #create a copy of digitdata with only round entries in the specified numeric column
  round_digitdata = digitdata
  round_digitdata@cleaned = round_digitdata@cleaned[round_numbers_indexes, ]
  round_digitdata@numbers = round_digitdata@numbers[round_numbers_indexes, ]
  round_digitdata@left_aligned = round_digitdata@left_aligned[round_numbers_indexes, ]
  round_digitdata@right_aligned = round_digitdata@right_aligned[round_numbers_indexes, ]

  #create a copy of digitdata with only unround entries in the specified numeric column
  unround_digitdata = digitdata
  unround_digitdata@cleaned = unround_digitdata@cleaned[-round_numbers_indexes, ]
  unround_digitdata@numbers = unround_digitdata@numbers[-round_numbers_indexes, ]
  unround_digitdata@left_aligned = unround_digitdata@left_aligned[-round_numbers_indexes, ]
  unround_digitdata@right_aligned = unround_digitdata@right_aligned[-round_numbers_indexes, ]

  return(list(round_digitdata=round_digitdata, unround_digitdata=unround_digitdata))
}


################main function############
###A WRAPPER FUNCTION FOR ALL DIGIT TEST
#performs unpacking unround number test by performing all-digit place two-way chi square test vs Benford’s Law
#digitdata is the class object;
#unpacking_rounding_column specifies the data column (has to be numeric!) to split upon rounded
#and unrounded numbers to perform unpacking rounding test
#data_columns are the names of numerical columns of data to be analyzed (defaulted as 'all' to the entire number table)
#digit_places are the indexes of digit places desired to look at
###can be single digit for single digit test, or an array of digits for multiple digit test
###-1 can only appear alone as for last digit test
###e.g. digit_places = 'all', 5, -1, c(1,2,3), etc.
#omit_05 has three options: omit both 0 and 5->c(0,5); omit only 0->0 or c(0); and omit neither->NA (when no rounding test is performed)
#if analysis by groups is desired, break_out should specify the deisred category to break upon
#distribution can be 'Benford' or 'Uniform' or more ???
#if skip_last_digit is true, will omit last digit before analysis, since we don't want tests to overlap

unpack_round_numbers_test = function(digitdata, contingency_table, unpacking_rounding_column, data_columns='all', digit_places='all',
                                      skip_first_figit=FALSE, omit_05=c(0,5), break_out=NA, distribution='Benford', plot=TRUE,
                                      skip_last_digit=FALSE){

  #unpack by round numbers indexes in the specified column
  lst = get_round_unround_digitdata(digitdata, unpacking_rounding_column)
  round_digitdata = lst$round_digitdata
  unround_digitdata = lst$unround_digitdata

  #perform all digit tests for each digitdata object
  round_p_values = all_digits_test(round_digitdata, contingency_table, data_columns, digit_places, skip_first_figit,
                                   omit_05, break_out, distribution, plot, skip_last_digit)

  unround_p_values = all_digits_test(unround_digitdata, contingency_table, data_columns, digit_places, skip_first_figit,
                                     omit_05, break_out, distribution, plot, skip_last_digit)

  #merge the results
  p_values = rbind(round_p_values, unround_p_values)
  rownames(p_values) = c('round', 'unround')
  print('unpack round numbers test')
  print(p_values)
  return(p_values)
}


