# ############################################################
# #Testing for repeat test
# #Wenjun Chang
# #Summer 2020
# ############################################################
#
# #############prelim############
# #clear workspace
# rm(list = ls())
# #free up R memory
# gc()
# #load stuff
# library(digitanalysis)
# ##############################
#
# #test with data
# #load data input functions
# data_columns = c("ALEXP.Values")
# fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'
# DigitData = process_digit_data(filepath = fp, digit_columns = data_columns)
#
# #DigitData has to drop all columns with NA in ALEXP.Values
# indexes_with_valid_alexp_values = which(!(is.na(DigitData@cleaned$ALEXP.Values)))
# DigitData = make_sub_digitdata(DigitData, indexes_with_valid_alexp_values)
#
# #test repeat test
# data_columns = "ALEXP.Values"
# duplicate_matching_cols = c("ALEXP.Values")#, "YEAR", "DIST")#, "SECTOR")
# break_out ='DIST'
# break_out_grouping=NA
# round_digit_to_skip=NA#c(0,5)
#
# result = repeat_test(DigitData, data_columns, duplicate_matching_cols, break_out=break_out, break_out_grouping=break_out_grouping, round_digit_to_skip=round_digit_to_skip)
# result
#
# # order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'all')
# # result[order, ]
