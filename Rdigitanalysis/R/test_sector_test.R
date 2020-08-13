############################################################
#Testing for sector test
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

#load all functions for repeat test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\repeat_test.R')

#load all functions for sector test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\sector_test.R')


#test with data
#load data input functions
data_columns = c("ALEXP.Values")
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'

DigitData = make_class(filepath = fp, col_analyzing = data_columns)
contingency_table = load_Benford_table('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')

#DigitData has to drop all columns with NA in ALEXP.Values
indexes_with_valid_alexp_values = which(!(is.na(DigitData@cleaned$ALEXP.Values)))
DigitData = make_sub_digitdata(DigitData, indexes_with_valid_alexp_values)

#test sector test
data_columns = c("ALEXP.Values")
break_out ='DIST'
category = 'SECTOR'
duplicate_matching_cols = c("ALEXP.Values", "YEAR", "DIST", "SECTOR")
category_grouping = list(Training_and_Transport=c("TRN", "TRAVEL", "VEHICLES"), Civil_Works=c("CW"), Goods_and_Equipment=c("GE"))
round_digit_to_skip = c(0,5)



result = sector_test(digitdata=DigitData, data_columns=data_columns, category=category, category_grouping=category_grouping, duplicate_matching_cols=duplicate_matching_cols,
                     break_out=break_out, round_digit_to_skip=round_digit_to_skip, plot=TRUE)


order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'All')
result$percent_repeats[order, ]

#result
