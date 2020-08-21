# ############################################################
# #Testing for digit pair test
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
# #load data input functions
# data_columns = c("BENTOT.Values")
# fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'
#
# DigitData = process_digit_data(filepath = fp, digit_columns = data_columns)
#
# #test digit pair test
# min_length = 3
# omit_05 = 0
# data_columns = "BENTOT.Values"
# break_out = 'DIST'
# break_out_grouping=NA
#
# result = digit_pairs_test(DigitData, data_columns, omit_05, min_length, break_out, break_out_grouping=break_out_grouping)
# order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'All')
# result$p_values
# result$p_values[order]
