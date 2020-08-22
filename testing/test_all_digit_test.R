############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# Testing for unpack unround column tests
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
data_columns = "BENTOT.Values"#'ALEXP.Values'#c("BENM", "BENF")#
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'

DigitData = process_digit_data(filepath = fp, digit_columns = data_columns)

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
category=NA#'YEAR'
category_grouping=NA
skip_last_digit=FALSE
suppress_low_N=FALSE
distribution = 'benford'
suppress_first_division_plots=F
suppress_second_division_plots=T

#match the data with Jetson's
result = all_digits_test(digitdata = DigitData, contingency_table = NA, data_columns = data_columns, digit_places = digit_places,
                         skip_first_digit = skip_first_digit, omit_05 = omit_05, break_out=break_out, break_out_grouping=break_out_grouping,
                         category=category, category_grouping=category_grouping, distribution=distribution, plot=F,
                         skip_last_digit = skip_last_digit, suppress_low_N=suppress_low_N, suppress_first_division_plots=suppress_first_division_plots,
                         suppress_second_division_plots=suppress_second_division_plots)
result$p_values
#result$plots$All
# order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'all')
# result[order]



