############################################################
#Functions for digit analysis R package
###plotting helper functions
#Wenjun Chang
#Summer 2020
############################################################

############################################################
#helper function
############################################################


#############prelim############
#clear workspace
rm(list = ls())
#free up R memory
gc()
#force numerical representation rather than scientific
#options(scipen = 999)
options(scipen = 1)
options(digits = 2)
##############################

#load data input functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\data_input_functions.R')

#load functions for computing Benford table
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\Benford_table_functions.R')

#load helper functions for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all_digit_test_helper_functions.R')

#load chi square test GOF functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\chi_square_goodness_of_fit_functions.R')

#load main function for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all_digit_test_main_function.R')

#load all functions for digit pair test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\digit_pair_test.R')

#load all functions for rounding test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\rounding_test.R')

#load all functions for repeat test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\repeat_test.R')

#load all functions for high low test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\high_low_test.R')

#load all functions for padding test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\padding_test.R')




#############################################################
#############try it with given data##########################
#############################################################

#test data input and benford table functions
#load data input functions
data_columns = c("ALEXP","BENTOT", "BENM", "BENF")
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'

DigitData = make_class(filepath = fp, col_analyzing = data_columns)

contingency_table = load_Benford_table('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')
contingency_table


#gg plot

#test padding test
data_columns = c("ALEXP")#,"BENTOT", "BENM", "BENF")
max_length = 7
num_digits = 5
N = 10 #120k datasets took 15 mins
omit_05 = c(0,5)
break_out = NA#'DIST'

a=padding_test(DigitData, contingency_table, data_columns, max_length, num_digits, N, omit_05, break_out)
a
a$diff_in_mean
a$p_values
a$expected_mean
a$observed_mean


library(ggplot2)
k=a$diff_in_mean
rownames(k) = 'value'
k['digit_places',] = colnames(k)

values = c(t(a$diff_in_mean))
digit_places = colnames(a$diff_in_mean)

mydata = data.frame(values, digit_places)

#2d
plot = ggplot(data = mydata, aes(x=digit_places, y=values)) +
  geom_bar(stat="identity") #+ geom_text(aes(label=values), vjust=-0.3, size=3.5, color='blue')
plot + theme(legend.position="bottom")

#2d with variables
b=padding_test(DigitData, contingency_table, data_columns, max_length, num_digits, N, omit_05, break_out="DIST")
p=data.frame(t(b$diff_in_mean[1:3, ]))
p
data = data.frame(matrix(nrow = 0, ncol = 3))
colnames(data) = c('digit_places', 'values', 'category')
data
for (name in colnames(p)){
  curr_data = data.frame(digit_places=rownames(p), values=p[[name]], category=rep(name, nrow(p)))
  data = rbind(data, curr_data)
}
data


# Stacked barplot with multiple groups
# Use position=position_dodge()
ggplot(data=data, aes(x=digit_places, y=values, fill=category)) +
  geom_bar(stat="identity", position=position_dodge())


data

#3d plot
ggplot(data, aes(x=digit_places, y=values, group=category, colour=category))
+ geom_line() + geom_point()

library(latticeExtra)
cloud(values~digit_places+category, data, panel.3d.cloud=panel.3dbars, col.facet='grey',
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1),
      par.settings = list(axis.line = list(col = "transparent")))


d <- read.table(text=' x   y     z
t1   5   high
t1   2   low
t1   4   med
t2   8   high
t2   1   low
t2   3   med
t3  50   high
t3  12   med
t3  35   low', header=TRUE)
cloud(y~x+z, d, panel.3d.cloud=panel.3dbars, col.facet='grey',
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1),
      par.settings = list(axis.line = list(col = "transparent")))
