############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# Check to see if t test stats is reproduced exactly as in repeats test
############################################################

fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'
data = read.csv(fp)
data = data[c("ALEXP.Values", "YEAR", "DIST", "SECTOR")]
data = data[!is.na(data[1]), ]
typeof(data[[1]])
numbers = stringi::stri_reverse(data[[1]])
data_omitted_ronding = data[-which(substr(numbers, start=1, stop=3) == '000'), ]

nrow(unique(data_omitted_ronding))
nrow(data_omitted_ronding)

current_data = data_omitted_ronding[data_omitted_ronding['DIST'] == 'Baringo', ]
other_data = data_omitted_ronding[data_omitted_ronding['DIST'] != 'Baringo', ]

current_counts = c(rep(0, nrow(unique(current_data))), rep(1, nrow(current_data)-nrow(unique(current_data))))
other_counts = c(rep(0, nrow(unique(other_data))), rep(1, nrow(other_data)-nrow(unique(other_data))))

t.test(current_counts, other_counts, alternative = "greater")$p.value
