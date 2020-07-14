############################################################
#Functions for digit analysis R package
###padding test functions in this file
#Wenjun Chang
#Summer 2020
############################################################

############################################################
#helper function
############################################################


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

#load all functions for high low test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\high low test.R')


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



