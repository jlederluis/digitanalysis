############################################################
#Functions for digit analysis R package
###Benford table create, save, and load function according to Benford's Law in this file
#Wenjun Chang
#Summer 2020
############################################################


#generate contingency table for Benford distribution (expected digit frequency under Benford's Law)
#for n digit places, and store it in a file for later use (as an option)
#In the contingency table, columnn are digit place in increasing order,
#whereas row is the digit (0 to 9) in increasing order
#gc() free up R memory cuz it might blow up
#N <= 6 takes roughly no time, but N > 6 are suspetible
#N = 7 takes 5 mins
#N = 8 takes 30 mins
Benford_table = function(N, out_fp, save=TRUE){

  contingency_table = data.frame(Digits=0:9)
  for (n in 1:N){
    if (n == 1){
      #first digit we hard code
      current_freqs = rep(0, 10)
      for (digit in 1:9){
        current_freqs[digit+1] = log10(1+1/digit)
      }
      #update table
      contingency_table[[paste('Digit Place', as.character(n))]] = NA
      contingency_table[[paste('Digit Place', as.character(n))]] = current_freqs
    }
    else {
      #digits place greater than the first
      args = list(1:9) #first digit has no 0
      args = append(args, rep(list(0:9), n-2))
      #create all possible combinations of the prefix digits
      n_digits_combinations = expand.grid(args)
      n_digits_combinations = apply(n_digits_combinations, 1, paste, collapse='')

      current_freqs = rep(NA, 10)
      for (digit in 0:9){
        freq = 0
        for (prefix in n_digits_combinations){
          denom = as.integer(paste(prefix,as.character(digit),sep=''))
          freq = freq + log10(1+1/as.integer(denom))
        }
        current_freqs[digit+1] = freq
      }
      #update table
      contingency_table[[paste('Digit Place', as.character(n))]] = current_freqs
    }
  }
  #drop the weird "X" column in df #############Guess it does not work, reload again it appears
  contingency_table = contingency_table[ , !(colnames(contingency_table) %in% c("X"))]

  #save file if specified
  if (save) {
    write.csv(contingency_table, out_fp)
  }
  return(contingency_table)
}

#
#Benford_table(N=8, out_fp='C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')

#load Benford table given filepath
load_Benford_table = function(fp){
  contingency_table = read.csv(fp)
  #get rid of '.' replacing ' ' problem when loading csv to df
  colnames(contingency_table) = gsub("."," ",colnames(contingency_table), fixed=TRUE)
  #name the rows from 0 to 9
  rownames(contingency_table) = 0:9
  return(contingency_table)
}


