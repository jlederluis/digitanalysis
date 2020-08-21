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
#load stuff
library(digitanalysis)
##############################

#test with data
#load data input functions
data_columns = c("ALEXP.Values")
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'
DigitData = process_digit_data(filepath = fp, digit_columns = data_columns)

#DigitData has to drop all columns with NA in ALEXP.Values
indexes_with_valid_alexp_values = which(!(is.na(DigitData@cleaned$ALEXP.Values)))
DigitData = make_sub_digitdata(DigitData, indexes_with_valid_alexp_values)

#test sector test
data_columns = c("ALEXP.Values")
break_out ='DIST'
category = 'SECTOR'
duplicate_matching_cols = c("ALEXP.Values", "YEAR", "DIST", "SECTOR")
category_grouping = NA#list(Training_and_Transport=c("TRN", "TRAVEL", "VEHICLES"), Civil_Works=c("CW"), Goods_and_Equipment=c("GE"))
round_digit_to_skip = NA#c(0,5)
category_instance_analyzing = 'TRN'
rounding_patterns_to_omit = '000'

result = sector_test(digitdata=DigitData, data_columns=data_columns, category=category, category_instance_analyzing=category_instance_analyzing,
                     category_grouping=category_grouping, duplicate_matching_cols=duplicate_matching_cols,
                     break_out=break_out, break_out_grouping=NA, rounding_patterns_to_omit=rounding_patterns_to_omit, plot=T)

# order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'All')
# result$percent_repeats[order, ]

