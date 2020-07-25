############################################################
#Functions for digit analysis R package
###plotting helper functions
#Wenjun Chang
#Summer 2020
############################################################

############################################################
#helper function
############################################################

#
# #############prelim############
# #clear workspace
# rm(list = ls())
# #free up R memory
# gc()
# #force numerical representation rather than scientific
# #options(scipen = 999)
# options(scipen = 1)
# options(digits = 2)
# ##############################
#
# #load data input functions
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\data_input_functions.R')
#
# #load functions for computing Benford table
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\Benford_table_functions.R')
#
# #load helper functions for all digit test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all_digit_test_helper_functions.R')
#
# #load chi square test GOF functions
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\chi_square_goodness_of_fit_functions.R')
#
# #load main function for all digit test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all_digit_test_main_function.R')
#
# #load all functions for digit pair test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\digit_pair_test.R')
#
# #load all functions for rounding test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\rounding_test.R')
#
# #load all functions for repeat test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\repeat_test.R')
#
# #load all functions for high low test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\high_low_test.R')
#
# #load all functions for padding test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\padding_test.R')
#
#
#
#
# #############################################################
# #############try it with given data##########################
# #############################################################
#
# #test data input and benford table functions
# #load data input functions
# data_columns = c("ALEXP","BENTOT", "BENM", "BENF")
# fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'
#
# DigitData = make_class(filepath = fp, col_analyzing = data_columns)
#
# contingency_table = load_Benford_table('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')
# contingency_table
#





#plot 2d histogram
#data style can be only 'row' or 'column'
#row means the rows has the data
#defaulted to row cuz this is how the tests returns data
#if want to add a horizontal line, hline specifies the y-intercept value
hist_2D = function(data, data_style='row', hline=NA){
  if (data_style == 'row'){
    #transpose it to column style, what ggplot wants
    data = data.frame(t(data))
  }
  plotting_data = data.frame(x=rownames(data), y=data)
  colnames(plotting_data) = c('digit_places', 'p_values') #ensure col name are correct
  print(plotting_data)

  #2d plot
  library(ggplot2)
  hist2d = ggplot(data=plotting_data, aes(x=digit_places, y=p_values)) +
    geom_bar(stat="identity")
  #+ geom_text(aes(label=values), vjust=-0.3, size=3.5, color='blue') #for label exact value

  if (!(is.na(hline))){
    #add the horizontal line desired
    hist2d = hist2d + geom_hline(yintercept=hline, color='red')
  }
  return(hist2d)
}


#plot 2d histogram with multiple varibales as specified by the break_out param
hist_2D_variables = function(data, data_style='row'){
  if (data_style == 'row'){
    #transpose it to column style, what ggplot wants
    data = data.frame(t(data))
  }
  #intialize a df for plotting, columns = x, y, category
  plotting_data = data.frame(matrix(nrow = 0, ncol = 3))
  colnames(plotting_data) = c('digit_places', 'p_values', 'category')

  #fill up df
  for (name in colnames(data)){
    single_category_data = data.frame(digit_places=rownames(data), p_values=data[[name]], category=rep(name, nrow(data)))
    plotting_data = rbind(plotting_data, single_category_data)
  }
  print(plotting_data)

  #stacked 2d barplot with multiple groups
  #use position=position_dodge()
  library(ggplot2)
  hist2d_multiple = ggplot(data=plotting_data, aes(x=digit_places, y=p_values, fill=category)) +
    geom_bar(stat="identity", position=position_dodge()) + scale_x_discrete(limits=rownames(data)) #ensure order of digit places
  #+ theme(legend.position="bottom") #legend position

  return(hist2d_multiple)
}


#plot multiple plots on a single image
#plot_list is a list of plots desire to show
plot_multiple_hist2d = function(plot_list){
  require(gridExtra)
  plots = do.call("grid.arrange", c(plots, nrow = floor(sqrt(length(plots)))))
  return(plots)
}

