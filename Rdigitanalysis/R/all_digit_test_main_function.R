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
#unpacking_rounding_column specifies the data column (has to be numeric!) to split upon rounded
#and unrounded numbers to perform unpacking rounding test


#!!!skip_last_digit should overwrite digit_places and (skip_first_digits)
all_digits_test = function(digitdata, contingency_table, data_columns='all', digit_places='all', skip_first_figit=FALSE,
                           omit_05=c(0,5), break_out=NA, distribution='Benford', plot=TRUE, skip_last_digit=FALSE,
                           unpacking_rounding_column=NA){

  #############some logical stuff to check and throw errors on###########
  #this must be multiple digits test
  if ((digit_places != 'all') && (length(digit_places) > 1 )){
    #thus should not have -1 as part of the array
    if (!is.na(match(-1, digit_places))){
      stop('multiple digits test cannot have last digit as part of the digit places')
    }
  }

  if (skip_first_figit){
    #should not have 1 as part of the array since we are skipping first digit place
    if (!is.na(match(1, digit_places))){
      stop('digit_places skip_first_figit contradicts, both looking and not looking at the first digit place')
    }
  }

  if (length(omit_05) == 1){
    ###check omit only 5, which is not allowed
    if (!(is.na(omit_05)) && (omit_05 == 5)){
      stop('cannot omit only 5 without also omitting 0 first')
    }
  }

  if (length(digit_places) == 1){
    #thus should not have -1 as part of the array
    if (skip_last_digit){
      stop('not using any data')
    }
    if (skip_first_figit){
      if (digit_places == 1){
        stop('not using any data')
      }
    }
  }

  #######################################################################
  #parse the data
  #######################################################################

  align_direction = 'left'

  #get the digits of the desired data columns to be analyzed
  lst = grab_desired_aligned_columns(digitdata, data_columns, skip_first_figit, skip_last_digit, align_direction)
  digitdata = lst$digitdata
  digits_table = lst$digits_table

  #get only the wanted digit places
  if (digit_places == 'all'){
    digit_places = seq(1, digitdata@max)
    if (skip_first_figit){
      digit_places = seq(2, digitdata@max)
    }
  }

  usable_data = parse_digit_places(digitdata, digits_table, digit_places)

  #parse only needed parts of contingency table
  contingency_table = parse_contigency_table(digitdata, contingency_table, digit_places, skip_first_figit, skip_last_digit, omit_05)

  #get observation table from usable data
  observation_table = obtain_observation(digitdata, usable_data, digit_places, skip_first_figit, skip_last_digit, omit_05)

  print("contingency_table")
  print(contingency_table)

  print("observation_table")
  print(observation_table)

  #######################################################################
  #do chi square test
  #######################################################################

  df = get_df(contingency_table)#, standard = TRUE) ##############

  print('degrees of freedom')
  print(df)

  #normal all digit test
  if (is.na(unpacking_rounding_column)){
    p_values = data.frame(all=chi_square_gof(observation_table, contingency_table, df))

    ##break on category if specified
    if (!(is.na(break_out))){
      #get indexes for each category
      indexes_of_categories = break_by_category(digitdata@cleaned, break_out) #this is a list since unequal number of entries for each category

      #breeak by category for all
      for (category_name in names(indexes_of_categories)){
        indexes_of_category = indexes_of_categories[[category_name]]
        obs_in_category = obtain_observation(digitdata, usable_data[indexes_of_category, ], digit_places, skip_first_figit, skip_last_digit, omit_05)
        p_values[category_name] = chi_square_gof(obs_in_category, contingency_table, df)
      }
    }
    print('results')
    print(p_values)
    return(p_values)
  }

  ##################################

  ##break on round unround
  ##perform unpack round digit test
  else {
    #intilaize a 2D table
    p_values = data.frame(matrix(nrow = 2, ncol = 0))
    rownames(p_values) = c('round', 'unround')

    #unpack by round numbers indexes
    round_numbers_indexes = unpacking_round_number_split(digitdata, unpacking_rounding_column)


    p_values[unpacking_rounding_column] = NA
    #perform chi square test on rounded rows
    #[round_numbers_indexes, ]
    obs_round = obtain_observation(digitdata, usable_data[round_numbers_indexes, ], digit_places, skip_first_figit, skip_last_digit, omit_05)
    p_values[unpacking_rounding_column]['round', ] = chi_square_gof(obs_round, contingency_table, df)

    #perform chi square test on unrounded rows
    #[-round_numbers_indexes, ]
    obs_unround = obtain_observation(digitdata, usable_data[-round_numbers_indexes, ], digit_places, skip_first_figit, skip_last_digit, omit_05)
    p_values[unpacking_rounding_column]['unround', ] = chi_square_gof(obs_unround, contingency_table, df)

    ##############################
    ##break on category if specified, then also need to break by category on round and unround
    if (!(is.na(break_out))){
      #get indexes for each category
      indexes_of_categories = break_by_category(digitdata@cleaned, break_out) #this is a list since unequal number of entries for each category

      #breeak by category for round, and unround
      for (category_name in names(indexes_of_categories)){
        p_values[category_name] = NA

        round_in_category_indexes = intersect(indexes_of_categories[[category_name]], round_numbers_indexes)
        obs_round_in_category = obtain_observation(digitdata, usable_data[round_in_category_indexes, ], digit_places, skip_first_figit, skip_last_digit, omit_05)
        p_values[category_name]['round', ] = chi_square_gof(obs_round_in_category, contingency_table, df)

        unround_in_category_indexes = setdiff(indexes_of_categories[[category_name]], round_in_category_indexes)
        obs_unround_in_category = obtain_observation(digitdata, usable_data[unround_in_category_indexes, ], digit_places, skip_first_figit, skip_last_digit, omit_05)
        p_values[category_name]['unround', ] = chi_square_gof(obs_unround_in_category, contingency_table, df)
      }
    }
  }
  print('results')
  print(p_values)
  return(p_values)
}

