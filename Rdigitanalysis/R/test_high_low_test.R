############################################################
#Testing for high low test
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

#load all functions for high low test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\high_low_test.R')


#test with data
#load data input functions
data_columns = c("ALEXP.Values")
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'

DigitData = make_class(filepath = fp, col_analyzing = data_columns)
contingency_table = load_Benford_table('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')

#DigitData has to drop all columns with NA in ALEXP.Values
indexes_with_valid_alexp_values = which(!(is.na(DigitData@cleaned$ALEXP.Values)))
DigitData = make_sub_digitdata(DigitData, indexes_with_valid_alexp_values)

#test high low test
data_columns = 'all'
high = c(6,7,8,9)
omit_05 = c(0,5)
skip_first_digit = TRUE
skip_last_digit = FALSE
break_out = 'DIST'
category = 'YEAR'
distribution='Benford'

#match the data with Jetson's
# result = single_high_low_test(digitdata=DigitData, contingency_table=contingency_table, data_columns=data_columns, high=high, omit_05=omit_05,
#                        skip_first_digit=skip_first_digit, skip_last_digit=skip_last_digit, category=category)


result = high_low_test(digitdata=DigitData, contingency_table=NA, data_columns=data_columns, high=high, omit_05=omit_05,
                       distribution=distribution, skip_first_digit=skip_first_digit, skip_last_digit=skip_last_digit, break_out=break_out, category=category, plot=T, test_type='chisq')

result
order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'All')
t(result[order, ]['2007'])


# a=result$high_digits_freq_table[1,]
# a =data.frame(a)
# theoratical = c(0.4546635,      0.495134,     0.4995116,     0.4999511,     0.4999951,     0.4999995)
# theoratical = data.frame(theoratical)
# rownames(theoratical) = rownames(a)
# theoratical
# dist_line = geom_line(data = data.frame(x=rownames(theoratical), y=theoratical[[1]]), aes(x = x, y = y, group=1, linetype='Expected High Digits Frequency'), color='red', lwd=1)
# hist_2D(a, data_style='col', xlab='Digit Place', ylab='High Digits Frequency', title='High Low Test', abline=dist_line, width=0.5)
# data=result$high_digits_freq_table
#
# hist_3d(data=data, digitdata=DigitData, xlab=break_out, ylab='digit places', zlab='p value', title='High Low Test', theta=55, phi=16)
