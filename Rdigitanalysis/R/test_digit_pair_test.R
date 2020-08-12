############################################################
#Testing for digit pair test
#Wenjun Chang
#Summer 2020
############################################################


############################################################
#Testing; run the functions
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

#load helper functions for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all_digit_test_helper_functions.R')

############################
#testing
#load all functions for digit pair test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\digit_pair_test.R')

#load data input functions
data_columns = "BENTOT.Values"#c("ALEXP.Values")
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'

DigitData = make_class(filepath = fp, col_analyzing = data_columns)
contingency_table = load_Benford_table('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')


#test digit pair test
min_length = 3
omit_05 = 0
data_columns = "BENTOT.Values"
break_out = 'DIST'
break_out_grouping=NA

result = digit_pairs_test(DigitData, data_columns, omit_05, min_length, break_out, break_out_grouping=break_out_grouping)
order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'All')
result[order]
