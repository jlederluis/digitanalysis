############################################################
#Functions for digit analysis R package
###all digit test main function in this file
#Wenjun Chang
#Summer 2020
############################################################


############################################################
#all digits test
############################################################


################main function############
#performs all-digit place two-way chi square test vs Benford’s Law
#digitdata is the class object;
#data_columns are the names of numerical columns of data to be analyzed (defaulted as 'all' to the entire number table)
#digit_places are the indexes of digit places desired to look at
###can be single digit for single digit test, or an array of digits for multiple digit test
###-1 can only appear alone as for last digit test
###e.g. digit_places = 'all', 5, -1, c(1,2,3), etc.
#omit_05 has three options: omit both 0 and 5->c(0,5); omit only 0->0 or c(0); and omit neither->NA (when no rounding test is performed)
#if analysis by groups is desired, break_out should specify the deisred category to break upon
#distribution can be 'Benford' or 'Uniform' or more ???
#if skip_last_digit is true, will omit last digit before analysis, since we don't want tests to overlap

#!!!skip_last_digit should overwrite digit_places and (skip_first_digits)
all_digits_test = function(digitdata, contingency_table, data_columns='all', digit_places='all', skip_first_digit=FALSE,
                           omit_05=c(0,5), break_out=NA, distribution='Benford', plot=TRUE, skip_last_digit=FALSE){

  #check input
  input_check(digitdata=digitdata, contingency_table=contingency_table, data_columns=data_columns, digit_places=digit_places,
              skip_first_digit=skip_first_digit, omit_05=omit_05, break_out=break_out, distribution=distribution, plot=plot,
              skip_last_digit=skip_last_digit)

  # #############some logical stuff to check and throw errors on###########
  # if (class(digitdata)[1] != 'DigitAnalysis'){
  #   stop("digitdata must be an object in the class DigitAnalysis.Check function make_class()!")
  # }
  # #this must be multiple digits test
  # if ((digit_places != 'all') && (length(digit_places) > 1 )){
  #   #thus should not have -1 as part of the array
  #   if (!is.na(match(-1, digit_places))){
  #     stop('multiple digits test cannot have last digit as part of the digit places')
  #   }
  # }
  #
  # if (skip_first_digit){
  #   #should not have 1 as part of the array since we are skipping first digit place
  #   if (!is.na(match(1, digit_places))){
  #     stop('digit_places skip_first_digit contradicts, both looking and not looking at the first digit place')
  #   }
  # }
  #
  # if (length(omit_05) == 1){
  #   ###check omit only 5, which is not allowed
  #   if (!(is.na(omit_05)) && (omit_05 == 5)){
  #     stop('cannot omit only 5 without also omitting 0 first')
  #   }
  # }
  #
  # if (length(digit_places) == 1){
  #   #thus should not have -1 as part of the array
  #   if (skip_last_digit){
  #     stop('not using any data')
  #   }
  #   if (skip_first_digit){
  #     if (digit_places == 1){
  #       stop('not using any data')
  #     }
  #   }
  # }

  #######################################################################
  #parse the data
  #######################################################################

  align_direction = 'left'

  #get the digits of the desired data columns to be analyzed
  lst = grab_desired_aligned_columns(digitdata, data_columns, skip_first_digit, skip_last_digit, align_direction)
  digitdata = lst$digitdata
  digits_table = lst$digits_table

  #get only the wanted digit places
  if (digit_places == 'all'){
    digit_places = seq(1, digitdata@max)
    if (skip_first_digit){
      digit_places = seq(2, digitdata@max)
    }
  }

  usable_data = parse_digit_places(digitdata, digits_table, digit_places)

  #parse only needed parts of contingency table
  contingency_table = parse_contigency_table(digitdata, contingency_table, digit_places, skip_first_digit, skip_last_digit, omit_05)

  #get observation table from usable data
  observation_table = obtain_observation(digitdata, usable_data, digit_places, skip_first_digit, skip_last_digit, omit_05)

  print("contingency_table")
  print(contingency_table)

  print(sum(observation_table))

  print("observation_table")
  print(observation_table)

  #######################################################################
  #do chi square test
  #######################################################################

  df = get_df(contingency_table, standard = TRUE) ##############

  print('degrees of freedom')
  print(df)

  #all digit test
  p_values = data.frame(all=chi_square_gof(observation_table, contingency_table, df))

  ##break on category if specified
  if (!(is.na(break_out))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, break_out) #this is a list since unequal number of entries for each category

    #breeak by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]
      obs_in_category = obtain_observation(digitdata, usable_data[indexes_of_category, ], digit_places, skip_first_digit, skip_last_digit, omit_05)
      p_values[category_name] = chi_square_gof(obs_in_category, contingency_table, df)

      print(category_name)
      print(sum(obs_in_category))
    }
  }
  print('results')
  print(p_values)
  return(p_values)
}

