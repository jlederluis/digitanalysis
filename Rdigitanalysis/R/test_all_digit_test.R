############################################################
#Testing for unpack unround column tests
#Wenjun Chang
#Summer 2020
############################################################

#############prelim############
#clear workspace
rm(list = ls())
#free up R memory
gc()
##############################
#general functions
#load data input functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\data_input_functions.R')

#load functions for computing Benford table
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\Benford_table_functions.R')

#load all plotting functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\plotting_functions.R')

#load input check function
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\input_check_function.R')

############################
#testing
#load helper functions for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all_digit_test_helper_functions.R')

#load chi square test GOF functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\chi_square_goodness_of_fit_functions.R')

#load main function for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all_digit_test_main_function.R')


#test with data
#load data input functions
data_columns = c("ALEXP.Values")#c("BENM", "BENF")#
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'

DigitData = make_class(filepath = fp, col_analyzing = data_columns)
contingency_table = load_Benford_table('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')

#DigitData has to drop all columns with NA in ALEXP.Values
indexes_with_valid_alexp_values = which(!(is.na(DigitData@cleaned$ALEXP.Values)))
DigitData = make_sub_digitdata(DigitData, indexes_with_valid_alexp_values)


#test all digits test
data_columns = 'all'#'ALEXP.Values'#
digit_places =  'all'
skip_first_digit=TRUE
omit_05 = c(0,5)
break_out='DIST'
break_out_grouping=NA
skip_last_digit=FALSE
standard_df=TRUE
suppress_low_N=FALSE
distribution = 'benford'

#match the data with Jetson's
result = all_digits_test(digitdata = DigitData, contingency_table = NA, data_columns = data_columns, digit_places = digit_places,
                         skip_first_digit = skip_first_digit, omit_05 = omit_05, break_out=break_out, break_out_grouping=break_out_grouping,
                         distribution=distribution, plot=FALSE, skip_last_digit = skip_last_digit, standard_df=standard_df, suppress_low_N=suppress_low_N)
result
# order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'all')
# result[order]

