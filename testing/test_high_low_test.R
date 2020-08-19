# ############################################################
# #Testing for high low test
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
# data_columns = c("ALEXP.Values")
# fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'
# DigitData = process_digit_data(filepath = fp, digit_columns = data_columns)
#
# #DigitData has to drop all columns with NA in ALEXP.Values
# indexes_with_valid_alexp_values = which(!(is.na(DigitData@cleaned$ALEXP.Values)))
# DigitData = make_sub_digitdata(DigitData, indexes_with_valid_alexp_values)
#
# #test high low test
# data_columns = 'all'
# high = c(6,7,8,9)
# omit_05 = c(0,5)
# skip_first_digit = TRUE
# skip_last_digit = FALSE
# break_out = 'DIST'
# category = 'YEAR'
# distribution='Benford'
#
# #match the data with Jetson's
# result = high_low_test(digitdata=DigitData, contingency_table=NA, data_columns=data_columns, high=high, omit_05=omit_05,
#                        distribution=distribution, skip_first_digit=skip_first_digit, skip_last_digit=skip_last_digit, break_out=break_out, category=category, plot=T, test_type='chisq')
#
# result
# order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'All')
# t(result[order, ]['2007'])
#
# # a=result$high_digits_freq_table[1,]
# # a =data.frame(a)
# # theoratical = c(0.4546635,      0.495134,     0.4995116,     0.4999511,     0.4999951,     0.4999995)
# # theoratical = data.frame(theoratical)
# # rownames(theoratical) = rownames(a)
# # theoratical
# # dist_line = geom_line(data = data.frame(x=rownames(theoratical), y=theoratical[[1]]), aes(x = x, y = y, group=1, linetype='Expected High Digits Frequency'), color='red', lwd=1)
# # hist_2D(a, data_style='col', xlab='Digit Place', ylab='High Digits Frequency', title='High Low Test', abline=dist_line, width=0.5)
# # data=result$high_digits_freq_table
# #
# # hist_3d(data=data, digitdata=DigitData, xlab=break_out, ylab='digit places', zlab='p value', title='High Low Test', theta=55, phi=16)
