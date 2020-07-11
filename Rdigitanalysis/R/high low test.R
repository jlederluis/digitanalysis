############################################################
#Functions for digit analysis R package
###high to low digit test functions in this file
#Wenjun Chang
#Summer 2020
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
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\data input functions.R')

#load functions for computing Benford table
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\Benford table functions.R')

#load helper functions for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all digit test helper functions.R')

#load chi square test GOF functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\chi square goodness of fit functions.R')

#load main function for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all digit test main function.R')

#load all functions for digit pair test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\digit pair test.R')

#load all functions for rounding test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\rounding test.R')

#load all functions for repeat test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\repeat test.R')


#test data input and benford table functions
#load data input functions
data_columns = c("ALEXP","BENTOT", "BENM", "BENF")
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'

DigitData = make_class(filepath = fp, col_analyzing = data_columns)

contingency_table = load_Benford_table('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')
contingency_table

high = c('6', '7', '8', '9')
omit_05 = c(0, 5)




# #overthinked
# #+1 since digit start at 0 but index start at 1
# high_digit_counts = counts_obs[intersect(names(counts_obs), high)] #the high digits counts for each present in this digit place
# high_digits_indexes = as.integer(intersect(names(counts_obs), high)) + 1 #the high digits indexes in contingency table
#
# low_digit_counts = counts_obs[setdiff(names(counts_obs), high)] #the low digits counts for each present in this digit place
# low_digits_indexes = as.integer(setdiff(names(counts_obs), high)) + 1 #the low digits indexes in contingency table
# high_digits_indexes
# low_digits_indexes
#
# high_digit_counts
# low_digit_counts
#
#
#
# counts_obs
# sum(counts_obs)
#
# high_freq_theoratical = sum(contingency_table[[1]][high_digits_indexes]*high_digit_counts)
#
#
# low_freq_theoratical = sum(contingency_table[[1]][low_digits_indexes]*low_digit_counts)
#
# sum(contingency_table[[1]][as.integer(names(counts_obs))+1]*counts_obs)
# sum(contingency_table[[1]][-(as.integer(names(counts_obs))+1)])


#get table for the theoratical high to low freqency in each digit place
#if omit_05 in high, then should throw error...no way should it be omiited and counted as highh digit
#drop X and Digits column of contingency table
high_freq_theoratical = contingency_table[!(colnames(contingency_table) %in% c('X', 'Digits'))]


#get the frequency for high digits in each digit place
high_freq_theoratical = t(data.frame(colSums(high_freq_theoratical[as.integer(high)+1,]))) #+1 since digit start at 0 but index start at 1
rownames(high_freq_theoratical) = 'high digits freq'

high_freq_theoratical[1]



data = single_column_aligned(DigitData, "ALEXP", 'left')
#i is the ith digit place always since it is from the left
for (i in 1:length(data)){

  #get frequency of each digit in each digit place
  counts_obs = table(data[i])
  counts_obs = counts_obs[-omit_05]

  #get freqency of high and low digit places
  high_counts_obs = sum(counts_obs[high])
  total_counts_obs = sum(counts_obs)

  #get p_value from binomial test
  p_value = binom.test(high_counts_obs, total_counts_obs, high_freq_theoratical[i])$p.value

  print(p_value)

}

high_low_counts_obs[1,]
as.array(high_low_counts_obs)
