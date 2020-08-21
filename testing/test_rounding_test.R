############################################################
#Testing for digit pair test
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

#load data input functions
data_columns = c("ALEXP.Values")
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'
DigitData = process_digit_data(filepath = fp, digit_columns = data_columns)

#test rounding test
rounding_patterns = c('0','00','000','0000', '00000', '000000', '5', '50', '500')
data_columns = c("ALEXP.Values")
break_out = 'DIST'
break_out_grouping=NA
result=rounding_test(DigitData, data_columns=data_columns, rounding_patterns=rounding_patterns, break_out=break_out, break_out_grouping=break_out_grouping)

# order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'All')
# result$percent_rounded[order, ]

