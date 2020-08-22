pkgname <- "Rdigitanalysis"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "Rdigitanalysis-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('Rdigitanalysis')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Benford_table")
### * Benford_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Benford_table
### Title: Generates contingency table for Benford distribution (a.k.a
###   expected digit frequency under Benford's Law). Columnn are digit
###   place in increasing order, and rows are the digits (0 to 9) in
###   increasing order
### Aliases: Benford_table

### ** Examples

Benford_table(N)
Benford_table(N, out_fp='~/filepath', save=TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Benford_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("all_digits_test")
### * all_digits_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: all_digits_test
### Title: Performs all-digit-place two-way chi square test vs
###   Benford<e2><80><99>s Law
### Aliases: all_digits_test

### ** Examples

all_digits_test(digitdata, contingency_table, data_columns='all', digit_places='all', skip_first_digit=TRUE)
all_digits_test(digitdata, contingency_table, data_columns='c(col_name1, col_name2)', digit_places=c(1,2,3,5), omit_05=NA, skip_last_digit=TRUE)
all_digits_test(digitdata, contingency_table, data_columns='all', digit_places=-1, omit_05=0, break_out='col_name', distribution='Uniform')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("all_digits_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("digit_pairs_test")
### * digit_pairs_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: digit_pairs_test
### Title: Performs terminal digit pair binomial test vs uniform
###   distribution (Benford<e2><80><99>s Law)
### Aliases: digit_pairs_test

### ** Examples

digit_pairs_test(digitdata, data_columns='all')
digit_pairs_test(digitdata, data_columns=c('col_name1', 'col_name2'))
digit_pairs_test(digitdata, data_columns='all', omit_05=NA, min_length=5)
digit_pairs_test(digitdata, data_columns='all', omit_05=0, break_out='col_name')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("digit_pairs_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("high_low_test")
### * high_low_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: high_low_test
### Title: Performs high to low digit tests vs probability of high to low
###   digits by Benford's Law via binomial test
### Aliases: high_low_test

### ** Examples

high_low_test(digitdata, contingency_table, data_columns='all', high=c(6,7,8,9))
high_low_test(digitdata, contingency_table, data_columns='all', high=c(8,9), skip_first_digit=TRUE)
high_low_test(digitdata, contingency_table, data_columns='all', high=c(5,6,9), omit_05=0, skip_last_digit=TRUE, break_out=NA)
high_low_test(digitdata, contingency_table, data_columns='all', high=9, omit_05=NA, skip_last_digit=TRUE, break_out='col_name')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("high_low_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("make_class")
### * make_class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: make_class
### Title: Create an object instance for 'DigitAnalysis'. Parse and clean
###   the data for digit analysis.
### Aliases: make_class

### ** Examples

make_class('col_name', filepath='~/filename.csv')
make_class('col_name', filepath='~/filename.xlsx', filetype='excel', delim=',')
make_class('col_name', raw_df=my_dataframe)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("make_class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("padding_test")
### * padding_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: padding_test
### Title: Performs padding test vs stimulations of Benford conforming
###   datasets via percentile
### Aliases: padding_test

### ** Examples

padding_test(digitdata, contingency_table, data_columns='all')
padding_test(digitdata, contingency_table, data_columns=c('col_name1', 'col_name2'), omit_05=NA)
padding_test(digitdata, contingency_table, data_columns='all', max_length=7, num_digits=3, omit_05=0)
padding_test(digitdata, contingency_table, data_columns='all', N=100, omit_05=NA, break_out='col_name')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("padding_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("repeat_test")
### * repeat_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: repeat_test
### Title: Performs repeat test or sector effect test
### Aliases: repeat_test

### ** Examples

repeat_test(digitdata)
repeat_test(digitdata, duplicate_matching_cols=c('col_name1, col_name2'))
repeat_test(digitdata, duplicate_matching_cols=c('col_name1, col_name2'), break_out='col_name')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("repeat_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rounding_test")
### * rounding_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rounding_test
### Title: Performs rounding test vs uniform distribution across categories
###   in a specified data column
### Aliases: rounding_test

### ** Examples

rounding_test(digitdata)
rounding_test(digitdata, omit_05=0)
rounding_test(digitdata, omit_05=NA, break_out='col_name')
rounding_test(digitdata, data_columns=c('col_name1', 'col_name2'))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rounding_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sector_test")
### * sector_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sector_test
### Title: Performs sector test to analyze uneven distribution of percent
###   repeats across sectors (supposed to be uniform). A wrapper function
###   for 'repeat_test'.
### Aliases: sector_test

### ** Examples

sector_test(digitdata, sector_column='sector_name', sector_grouping=list('sector 1'=c('a'), 'sector 2'=c('b', 'c')))
sector_test(digitdata, sector_column='sector_name', sector_grouping=list('sector 1'=c('a, b'), 'sector 2'=c('c', 'd')),
duplicate_matching_cols=c('col_name1, col_name2'), break_out='col_name', failure_factor=3)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sector_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("single_column_aligned")
### * single_column_aligned

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: single_column_aligned
### Title: Fetches the left/right aligned table for a single numeric data
###   column
### Aliases: single_column_aligned

### ** Examples

single_column_aligned(digitdata, 'col_name1', align_diretion='left')
single_column_aligned(digitdata, 'col_name2', align_diretion='right')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("single_column_aligned", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("unpack_round_numbers_test")
### * unpack_round_numbers_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: unpack_round_numbers_test
### Title: Performs unpacking unround number test by performing all-digit
###   place two-way chi square test vs Benford<e2><80><99>s Law. A wrapper
###   function for 'all_digit_test'.
### Aliases: unpack_round_numbers_test

### ** Examples

unpack_round_numbers_test(digitdata, contingency_table, unpacking_rounding_column='Column Name', data_columns='all', digit_places='all')
unpack_round_numbers_test(digitdata, contingency_table, unpacking_rounding_column='Column Name', data_columns='all', digit_places=-1)
unpack_round_numbers_test(digitdata, contingency_table, unpacking_rounding_column='Column Name', data_columns='all', digit_places=c(1,2,3), omit_05=NA)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("unpack_round_numbers_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
