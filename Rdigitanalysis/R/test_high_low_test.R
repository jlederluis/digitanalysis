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

#match the data with Jetson's
# result = single_high_low_test(digitdata=DigitData, contingency_table=contingency_table, data_columns=data_columns, high=high, omit_05=omit_05,
#                        skip_first_digit=skip_first_digit, skip_last_digit=skip_last_digit, category=category)
#

result = high_low_test(digitdata=DigitData, contingency_table=contingency_table, data_columns=data_columns, high=high, omit_05=omit_05,
                       skip_first_digit=skip_first_digit, skip_last_digit=skip_last_digit, break_out=break_out, category=category, plot=T)

result


# #weird columns
# result = result[!(rownames(result) %in% c('all', 'Turkana')), ]
# result
# rowMeans(result)
# # order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'all')
# # result[order]
#
hist_3d = function(data, digitdata, xlab='digits', ylab='digit places', zlab='frequency', title='3D Bar Plot', theta=55, phi=16, save=FALSE){
  #assert digitdata is of correct class
  input_check(digitdata=digitdata)

  x = 1:length(rownames(data))
  y = as.numeric(which(digitdata@left_aligned_column_names %in% colnames(data)))

  z = as.matrix(data)
  plot3D::hist3D(x=x, y=y, z=z, zlim=c(0,max(z, na.rm=TRUE)+0.01), bty = "b2", theta=theta, phi=phi, axes=TRUE, label=TRUE, nticks=max(length(x),length(y)),
                 ticktype="detailed", space=0, expand=0.5, d=2, col='grey', colvar=NA, border='black', shade=0,
                 lighting=list('ambient'=0.6, 'diffuse'=0.6), main=title, xlab=xlab, ylab=ylab, zlab=zlab)

  if (save){
    filename = paste(title, ".pdf", sep='')
    print(filename)
    pdf(file = filename)
    plot3D::hist3D(x=x, y=y, z=z, zlim=c(0,max(z, na.rm=TRUE)+0.01), bty = "b2", theta=theta, phi=phi, axes=TRUE, label=TRUE, nticks=max(length(x),length(y)),
                   ticktype="detailed", space=0, expand=0.5, d=2, col='grey', colvar=NA, border='black', shade=0,
                   lighting=list('ambient'=0.6, 'diffuse'=0.6), main=title, xlab=xlab, ylab=ylab, zlab=zlab)#, cex.axis = 1e-9)
    # plot3D::text3D(x = 1:length(x)+0.3, y = rep(1.15, length(x)), z = rep(0, length(x)), labels = x, add = TRUE, adj = 0)
    # plot3D::text3D(x = rep(0, length(y)), y = 1:length(y)+0.5, z = rep(1, length(y)), labels = y, add = TRUE, adj = 1)
    dev.off()
  }
}
#
# hist_3d(data=result, digitdata=DigitData, xlab=break_out, ylab='digit places', zlab='p value', title='High Low Test', theta=55, phi=16)
