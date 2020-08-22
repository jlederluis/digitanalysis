



fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'
data = read.csv(fp)
data = data[c("ALEXP.Values", "DIST")]
data = data[!is.na(data[1]), ]
typeof(data[[1]])
numbers = stringi::stri_reverse(data[[1]])

data_category = data[data['DIST'] == 'Ijara', ][[1]]
data_others = data[data['DIST'] != 'Ijara', ][[1]]
nrow(data)
length(data_category)
length(data_others)
stringi::stri_reverse(data_others)

rounding_patterns = c('0','00','000','0000', '00000', '000000', '5', '50', '500')
rounding_patterns = stringi::stri_reverse(rounding_patterns[order(nchar(rounding_patterns), rounding_patterns, decreasing=TRUE)])
rounding_patterns

compute_rounding = function(data, rounding_patterns){
  total_digits = nchar(data)
  rounded_digits = rep(0, length(data))
  data = stringi::stri_reverse(data)
  for (pattern in rounding_patterns){
    indexes = which(substr(data, start=1, stop=nchar(pattern)) == pattern)
    if (length(indexes) > 0){
      #update rounded digits: always matches the lonegest pattern --> max
      for (index in indexes){
        #ugly for loop but dont know how to vectorize
        rounded_digits[index] = max(rounded_digits[index], nchar(pattern))
      }
    }
  }
  return(rounded_digits/total_digits)
}

result_category = compute_rounding(data_category, rounding_patterns)
result_others = compute_rounding(data_others, rounding_patterns)
t.test(result_category, result_others, alternative = "greater")$p.value
